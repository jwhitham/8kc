with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;

with backend;
with lex;

package body grammar is

	function expect_operator return Boolean;
	function expect_expr return Boolean;
	function expect_ifcond return Boolean;
	function expect_stmt (stmt : t_token) return Boolean;
	function expect_global (glob : t_token) return Boolean;

	procedure assert_expected (token : t_token; kind : t_token_kind) is
	begin
		if token.kind /= kind then
			raise user_error with "expected a " & kind'Img & ", got a " & token.kind'Img;
		end if;
	end assert_expected;

	function expect_operator return Boolean is
		t : constant t_token := lex.get_token;
		rc : Boolean := True;
	begin
		backend.comment ("; +expect_operator " & t.kind'Img);
		case t.kind is
			when alu_op =>
				rc := expect_expr;
				backend.gen_instruction (kind => t.op, binary => True);
			when eclose | sclose | eot | semicolon =>
				rc := False;
			when number | eopen | sopen | error | var | set | symbol | defining_symbol | print | wloop
					| ifcond .. elsecond | func =>
				raise user_error with "expected an operator, got a " & t.kind'Img;
		end case;
		backend.comment ("; -expect_operator " & t.kind'Img);
		return rc;
	end expect_operator;

	function expect_expr return Boolean is
		t : constant t_token := lex.get_token;
		rc : Boolean := True;
	begin
		backend.comment ("; +expect_expr " & t.kind'Img);
		case t.kind is
			when number =>
				backend.gen_instruction (push, t.value);
				rc := expect_operator;
			when eopen =>
				while expect_expr and then expect_operator loop
					null;
				end loop;
			when alu_op =>
				rc := expect_expr;
				backend.gen_instruction (kind => t.op, binary => False); -- unary op
			when set | var | print | wloop | sopen | ifcond .. elsecond | func =>
				raise user_error with "expected an expression, got a " & t.kind'Img;
			when defining_symbol =>
				raise user_error with "symbol is undefined";
			when symbol =>
				backend.gen_instruction (load, t.sym);
				rc := expect_operator;
			when eot | semicolon | eclose | sclose =>
				rc := False;
			when error =>
				raise user_error;
		end case;
		backend.comment ("; -expect_expr " & t.kind'Img);
		return rc;
	end expect_expr;

	function expect_ifcond return Boolean is
		footer 	: constant t_label_id := backend.new_label;
		next	: t_label_id := backend.new_label;
		rc 		: Boolean := True;
	begin
		rc := expect_expr;
		backend.gen_instruction (branch_if_zero, next);
		if not expect_stmt (lex.get_token) then
			raise user_error with "cannot end if block here";
		end if;
		loop
			declare
				stmt2 : constant t_token := lex.get_token;
			begin
				case stmt2.kind is
					when elsifcond =>
						-- end previous if block
						backend.gen_instruction (branch_always, footer);
						-- new test begins
						backend.gen_instruction (label, next);
						next := backend.new_label;
						rc := expect_expr;
						backend.gen_instruction (branch_if_zero, next);
						-- new if block
						if not expect_stmt (lex.get_token) then
							raise user_error with "cannot end elsif block here";
						end if;
					when elsecond =>
						-- end previous if block
						backend.gen_instruction (branch_always, footer);
						-- final if block
						backend.gen_instruction (label, next);
						next := backend.new_label;
						if not expect_stmt (lex.get_token) then
							raise user_error with "cannot end else block here";
						end if;
					when others =>
						-- end final if block
						backend.gen_instruction (label, next);
						backend.gen_instruction (label, footer);
						-- do whatever is next
						return expect_stmt (stmt2);
				end case;
			end;
		end loop;
	end expect_ifcond;

	function expect_stmt (stmt : t_token) return Boolean is
		rc : Boolean := True;
		pop_count : Natural := 0;
	begin
		backend.comment ("; +expect_stmt " & stmt.kind'Img);
		case stmt.kind is
			when var =>
				raise user_error with "var only allowed at the beginning of the block";
			when print =>
				rc := expect_expr;
				backend.gen_instruction (print);
			when set =>
				declare
					left : constant t_token := lex.get_token;
				begin
					assert_expected (left, symbol);
					rc := expect_expr;
					backend.gen_instruction (store, left.sym);
				end;
			when wloop =>
				declare
					header : constant t_label_id := backend.new_label;
					footer : constant t_label_id := backend.new_label;
				begin
					backend.gen_instruction (label, header);
					rc := expect_expr;
					backend.gen_instruction (branch_if_zero, footer);
					if not expect_stmt (lex.get_token) then
						raise user_error with "cannot end while block here";
					end if;
					backend.gen_instruction (branch_always, header);
					backend.gen_instruction (label, footer);
				end;
			when ifcond =>
				if not expect_ifcond then
					backend.comment ("; -expect_stmt " & stmt.kind'Img);
					return False;
				end if;
			when alu_op | defining_symbol | symbol | number | elsifcond | elsecond
					| eopen | eclose | func =>
				raise user_error with "expected a statement, got a " & stmt.kind'Img;
			when sopen =>
				-- look for var declarations
				loop
					declare
						first : constant t_token := lex.get_token;
					begin
						if first.kind = var then
							declare
								def : constant t_token := lex.get_token;
								init : constant t_value := 100 + t_value (pop_count);
							begin
								assert_expected (def, defining_symbol);
								assert_expected (lex.get_token, semicolon);
								backend.gen_instruction (push, init);
								def.sym.offset := backend.get_current_offset;
								pop_count := pop_count + 1;
							end;
						else
							rc := expect_stmt (first);
							exit;
						end if;
					end;
				end loop;
				while rc loop
					rc := expect_stmt (lex.get_token);
				end loop;
				while pop_count /= 0 loop
					backend.gen_instruction (pop);
					pop_count := pop_count - 1;
				end loop;
			when sclose =>
				backend.comment ("; -expect_stmt " & stmt.kind'Img);
				return False;
			when semicolon =>
				null;
			when eot =>
				raise end_program;
			when error =>
				raise user_error;
		end case;
		backend.comment ("; -expect_stmt " & stmt.kind'Img);
		return True;
	end expect_stmt;

	function expect_global (glob : t_token) return Boolean is
		rc : Boolean := True;
		pop_count : Natural := 0;
	begin
		backend.comment ("; +expect_global " & glob.kind'Img);
		case glob.kind is
			when var =>
				raise user_error with "global variables not supported yet";
			when func =>
				declare
					def : constant t_token := lex.get_token;
					follow : constant t_token := lex.get_token;
				begin
					case def.kind is
						when defining_symbol | symbol =>
							-- function body already defined?
							case follow.kind is
								when sopen =>
									if def.sym.has_body then
										raise user_error with "function body already defined";
									end if;
									def.sym.has_body := True;
									backend.gen_instruction (func_begin, To_String (def.sym.name));
									rc := expect_stmt (follow);
									backend.gen_instruction (func_end, To_String (def.sym.name));
								when semicolon =>
									null;
								when others =>
									raise user_error with "expected function body or ;";
							end case;
						when others =>
							raise user_error with "expected function name";
					end case;
				end;
			when print | set | wloop | ifcond
					| alu_op | defining_symbol | symbol | number
					| elsifcond | elsecond | eopen | eclose
					| sopen | sclose | semicolon =>
				raise user_error with "expected a global, got a " & glob.kind'Img;
			when eot =>
				raise end_program;
			when error =>
				raise user_error;
		end case;
		backend.comment ("; -expect_global " & glob.kind'Img);
		return True;
	end expect_global;

	procedure parse is
	begin
		while expect_global (lex.get_token) loop
			null;
		end loop;
	exception
		when end_program =>
			null;
		when e : user_error =>
			Ada.Text_IO.Put_Line
				(Ada.Text_IO.Standard_Error,
				 "at line" & Ada.Text_IO.Count'Image (Ada.Text_IO.Line (Ada.Text_IO.Standard_Input))
				 & " column" & Ada.Text_IO.Count'Image (Ada.Text_IO.Col (Ada.Text_IO.Standard_Input))
				 & ": " & Ada.Exceptions.Exception_Message (e));
			raise;
	end parse;

end grammar;

