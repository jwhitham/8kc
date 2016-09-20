with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;

with types; use types;
with backend;

procedure expr is
	char 		: Character := ' ';
	text 		: Unbounded_String;
	undo_char 	: Character := Ada.Characters.Latin_1.NUL;

	end_program	: exception;
	symbols		: types.p_symbols.Map;

	procedure update_char is
	begin
		if undo_char /= Ada.Characters.Latin_1.NUL then
			char := undo_char;
			undo_char := Ada.Characters.Latin_1.NUL;
			return;
		end if;
		Ada.Text_IO.Get (char);
	--	Ada.Text_IO.Put (char);
	exception
		when Ada.Text_IO.End_Error =>
			char := Ada.Characters.Latin_1.EOT;
	end update_char;

	procedure unupdate_char is
	begin
		if undo_char /= Ada.Characters.Latin_1.NUL then
			raise Program_Error with "can't undo twice in succession, sorry";
		end if;
		undo_char := char;
		char := Ada.Characters.Latin_1.NUL;
	end unupdate_char;

	procedure update_text is
	begin
		text := Null_Unbounded_String;
		Append (text, char);
		loop
			update_char;
			exit when char not in 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9';
			Append (text, char);
		end loop;
		unupdate_char;
	end update_text;

	function lex2 return t_token is
		c : p_symbols.Cursor;
	begin
		outer : loop
			update_char;
			case char is
				when '(' =>
					return (kind => eopen);
				when ')' =>
					return (kind => eclose);
				when '{' =>
					return (kind => sopen);
				when '}' =>
					return (kind => sclose);
				when '*' =>
					return (kind => alu_op, op => op_mul);
				when '/' =>
					case char is
						when '/' =>
							return (kind => alu_op, op => op_idiv);
						when others =>
							unupdate_char;
							return (kind => alu_op, op => op_div);
					end case;
				when '+' =>
					return (kind => alu_op, op => op_add);
				when '-' =>
					return (kind => alu_op, op => op_sub);
				when '>' =>
					update_char;
					case char is
						when '>' =>
							return (kind => alu_op, op => op_srl);
						when '=' =>
							return (kind => alu_op, op => op_ge);
						when '/' =>
							return (kind => alu_op, op => op_sra);
						when others =>
							unupdate_char;
							return (kind => alu_op, op => op_gt);
					end case;
				when '!' =>
					update_char;
					case char is
						when '=' =>
							return (kind => alu_op, op => op_ne);
						when others =>
							unupdate_char;
							return (kind => alu_op, op => op_lnot);
					end case;
				when '~' =>
					return (kind => alu_op, op => op_bnot);
				when '<' =>
					update_char;
					case char is
						when '<' =>
							return (kind => alu_op, op => op_sll);
						when '=' =>
							return (kind => alu_op, op => op_le);
						when others =>
							unupdate_char;
							return (kind => alu_op, op => op_lt);
					end case;
				when '|' =>
					update_char;
					case char is
						when '|' =>
							return (kind => alu_op, op => op_bor);
						when others =>
							unupdate_char;
							return (kind => alu_op, op => op_lor);
					end case;
				when '^' =>
					update_char;
					case char is
						when '^' =>
							return (kind => alu_op, op => op_bxor);
						when others =>
							unupdate_char;
							return (kind => alu_op, op => op_lxor);
					end case;
				when '&' =>
					update_char;
					case char is
						when '&' =>
							return (kind => alu_op, op => op_band);
						when others =>
							unupdate_char;
							return (kind => alu_op, op => op_land);
					end case;
				when '=' =>
					update_char;
					case char is
						when '=' =>
							return (kind => alu_op, op => op_eq);
						when others =>
							raise user_error with "single = is not allowed";
					end case;
				when ';' =>
					return (kind => semicolon);
				when 'a' .. 'z' | 'A' .. 'Z' =>
					update_text;
					c := symbols.Find (text);
					if p_symbols.Has_Element (c) then
						-- symbol previously seen
						return (kind => symbol, sym => p_symbols.Element (c));
					elsif To_String (text) = "var" then
						-- variable declaration keyword
						return (kind => var);
					elsif To_String (text) = "set" then
						-- assignment keyword
						return (kind => set);
					elsif To_String (text) = "if" then
						return (kind => ifcond);
					elsif To_String (text) = "elsif" then
						return (kind => elsifcond);
					elsif To_String (text) = "else" then
						return (kind => elsecond);
					elsif To_String (text) = "print" then
						return (kind => print);
					elsif To_String (text) = "while" then
						return (kind => wloop);
					elsif To_String (text) = "func" then
						return (kind => func);
					else
						-- symbol not previously seen
						declare
							sym : constant t_symbol_p := new t_symbol;
						begin
							sym.symbol_id := t_symbol_id (symbols.Length);
							sym.name := text;
							symbols.Insert (text, sym);
							return (kind => defining_symbol, sym => sym);
						end;
					end if;
				when '0' .. '9' =>
					declare
						value : t_value := 0;
					begin
						value := Character'Pos (char) - Character'Pos ('0');
						inner : loop
							update_char;
							exit inner when char not in '0' .. '9';
							value := value * 10;
							value := value + Character'Pos (char) - Character'Pos ('0');
						end loop inner;
						unupdate_char;
						return (kind => number, value => value);
					end;
				when ' ' | Ada.Characters.Latin_1.HT
						| Ada.Characters.Latin_1.CR
						| Ada.Characters.Latin_1.LF =>
					null;
				when Ada.Characters.Latin_1.EOT =>
					return (kind => eot);
				when others =>
					raise user_error with "invalid character: " & char;
			end case;
		end loop outer;
	end lex2;

	function lex return t_token is
		t : constant t_token := lex2;
	begin
		backend.comment
		   ("; " & t.kind'Img
			& " at line" & Ada.Text_IO.Count'Image (Ada.Text_IO.Line (Ada.Text_IO.Standard_Input))
			& " column" & Ada.Text_IO.Count'Image (Ada.Text_IO.Col (Ada.Text_IO.Standard_Input)));
		return t;
	end lex;

	procedure expect (token : t_token; kind : t_token_kind) is
	begin
		if token.kind /= kind then
			raise user_error with "expected a " & kind'Img & ", got a " & token.kind'Img;
		end if;
	end expect;

	function expect_expr return Boolean;

	function expect_operator return Boolean is
		t : constant t_token := lex;
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
		t : constant t_token := lex;
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

	function expect_stmt (stmt : t_token) return Boolean;

	function expect_ifcond return Boolean is
		footer 	: constant t_label_id := backend.new_label;
		next	: t_label_id := backend.new_label;
		rc 		: Boolean := True;
	begin
		rc := expect_expr;
		backend.gen_instruction (branch_if_zero, next);
		if not expect_stmt (lex) then
			raise user_error with "cannot end if block here";
		end if;
		loop
			declare
				stmt2 : constant t_token := lex;
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
						if not expect_stmt (lex) then
							raise user_error with "cannot end elsif block here";
						end if;
					when elsecond =>
						-- end previous if block
						backend.gen_instruction (branch_always, footer);
						-- final if block
						backend.gen_instruction (label, next);
						next := backend.new_label;
						if not expect_stmt (lex) then
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
					left : constant t_token := lex;
				begin
					expect (left, symbol);
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
					if not expect_stmt (lex) then
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
						first : constant t_token := lex;
					begin
						if first.kind = var then
							declare
								def : constant t_token := lex;
								init : constant t_value := 100 + t_value (pop_count);
							begin
								expect (def, defining_symbol);
								expect (lex, semicolon);
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
					rc := expect_stmt (lex);
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
					def : constant t_token := lex;
					follow : constant t_token := lex;
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
begin
	backend.init;
	begin
		while expect_global (lex) loop
			null;
		end loop;
	exception
		when end_program =>
			null;
	end;
	backend.fini;
exception
	when e : user_error =>
		Ada.Text_IO.Put_Line
			(Ada.Text_IO.Standard_Error,
			 "at line" & Ada.Text_IO.Count'Image (Ada.Text_IO.Line (Ada.Text_IO.Standard_Input))
			 & " column" & Ada.Text_IO.Count'Image (Ada.Text_IO.Col (Ada.Text_IO.Standard_Input))
			 & ": " & Ada.Exceptions.Exception_Message (e));
		raise;
end expr;

