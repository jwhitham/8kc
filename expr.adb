with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;
with Ada.Exceptions;

procedure expr is
	char 		: Character := ' ';
	text 		: Unbounded_String;
	undo_char 	: Character := Ada.Characters.Latin_1.NUL;

	type t_alu_op_kind is (op_add, op_sub, op_bor, op_band, op_bxor, op_sll, op_srl,
						op_sra, op_lt, op_le, op_gt, op_ge, op_eq, op_ne, op_mul,
						op_idiv, op_div, op_land, op_lxor, op_lor, op_lnot, op_bnot);
	type t_token_kind is (eopen, eclose, sopen, sclose, error, number, alu_op, print, wloop,
						semicolon, eot, symbol, set, var, defining_symbol,
						ifcond, elsifcond, elsecond);
	type t_instruction_kind is (push, unary_op, binary_op, load, store, print,
						branch_if_zero, branch_always, label, pop);
	type t_value is new Natural;
	type t_symbol_id is new Natural;
	type t_label_id is new Natural;
	type t_current_offset is new Natural;

	type t_symbol is record
		symbol_id : t_symbol_id := 0;
		offset    : t_current_offset := 0;
	end record;
	type t_symbol_p is access t_symbol;

	package p_symbols is new Ada.Containers.Hashed_Maps
		(Unbounded_String, t_symbol_p, Ada.Strings.Unbounded.Hash, "=", "=");

	user_error	: exception;
	end_program	: exception;
	symbols		: p_symbols.Map;
	num_labels	: t_label_id := 0;
	current_offset : t_current_offset := 0;

	type t_token (kind : t_token_kind) is record
		case kind is
			when number =>
				value		: t_value := 0;
			when symbol | defining_symbol =>
				sym      	: t_symbol_p := null;
			when alu_op =>
				op			: t_alu_op_kind := op_add;
			when others =>
				null;
		end case;
	end record;

	function new_label return t_label_id is
		l : constant t_label_id := num_labels;
	begin
		num_labels := num_labels + 1;
		return l;
	end new_label;

	procedure p (s : String) renames Ada.Text_IO.Put_Line;

	procedure gen_instruction (kind : t_instruction_kind; arg : String := "") is
	begin
		case kind is
			when push =>
				p ("push " & arg);
				current_offset := current_offset + 1;
			when unary_op | binary_op =>
				pragma Assert (False);
				null;
			when load =>
				p ("mov eax,[esp + (4 *" & arg & ")]");
				p ("push eax");
				current_offset := current_offset + 1;
			when store =>
				p ("pop eax");
				p ("mov [esp + (4 * (" & arg & " - 1))], eax");
				current_offset := current_offset - 1;
			when print =>
				p ("call print");
				p ("pop eax");
				current_offset := current_offset - 1;
			when branch_if_zero =>
				p ("pop eax");
				p ("cmp eax, 0");
				p ("je " & arg);
				current_offset := current_offset - 1;
			when branch_always =>
				p ("jmp " & arg);
			when label =>
				p (arg & ":");
			when pop =>
				p ("pop eax");
				current_offset := current_offset - 1;
		end case;
	end gen_instruction;

	procedure gen_instruction (kind : t_instruction_kind; label_id : t_label_id) is
		a : String := label_id'Img;
	begin
		a (a'First) := '_';
		gen_instruction (kind => kind, arg => "L" & a);
	end gen_instruction;

	procedure gen_instruction (kind : t_alu_op_kind; binary : Boolean) is
		procedure sign_fill (r : String := "eax") is
		begin
			p ("sar " & r & ", 31"); -- fill with sign bit
		end sign_fill;
		procedure eq (neq : Boolean) is
			label_id : constant t_label_id := new_label;
			text 	 : String := label_id'Img;
		begin
			text (text'First) := '_';
			p ("xor eax, ebx"); -- zero if equal
			if neq then
				p ("mov eax, -1");
			else
				p ("mov eax, 0");
			end if;
			p ("jnz B" & text);
			p ("not eax");
			p ("B" & text & ":");
		end eq;
	begin
		if not binary then
			p ("pop eax");
			case kind is
				when op_add =>
					null;
				when op_sub =>
					p ("neg eax");
				when op_lnot =>
					p ("not eax");
				when op_bnot =>
					p ("not eax");
				when others =>
					raise user_error with kind'Img & " is not a unary operation";
			end case;
			p ("push eax");
			return;
		end if;

		current_offset := current_offset - 1;
		p ("pop ebx");
		p ("pop eax");
		case kind is
			when op_add =>	p ("add eax, ebx");
			when op_sub =>	p ("sub eax, ebx");
			when op_sll =>	p ("sll eax, ebx");
			when op_srl =>	p ("srl eax, ebx");
			when op_sra =>	p ("sra eax, ebx");
			when op_mul =>	p ("imul eax, ebx");
			when op_div =>	p ("div eax, ebx");
			when op_idiv =>	p ("idiv eax, ebx");
			when op_lor =>	p ("or eax, ebx");
			when op_land =>	p ("and eax, ebx");
			when op_lxor =>	p ("xor eax, ebx");
			when op_bor =>	p ("or eax, ebx"); sign_fill;
			when op_band =>	p ("and eax, ebx"); sign_fill;
			when op_bxor =>	p ("xor eax, ebx"); sign_fill;
			when op_lt =>	p ("sub eax, ebx"); sign_fill;
			when op_gt =>	p ("sub ebx, eax"); sign_fill ("ebx"); p ("push ebx"); return;
			when op_ge =>	p ("sub eax, ebx"); p ("not eax"); sign_fill;
			when op_le =>	p ("sub ebx, eax"); p ("not ebx"); sign_fill ("ebx"); p ("push ebx"); return;
			when op_eq =>	eq (False);
			when op_ne =>	eq (True);
			when others =>
				raise user_error with kind'Img & " is not a binary operation";
		end case;
		p ("push eax");
	end gen_instruction;

	procedure gen_instruction (kind : t_instruction_kind; value : t_value) is
	begin
		gen_instruction (kind => kind, arg => value'Img);
	end gen_instruction;

	procedure gen_instruction (kind : t_instruction_kind; sym : t_symbol_p) is
	begin
		p ("; variable is at offset" & sym.offset'Img);
		p ("; current offset is" & current_offset'Img);
		if current_offset < sym.offset then
			raise user_error with "symbol is out of scope";
		end if;
		gen_instruction (kind => kind, value => t_value (current_offset - sym.offset));
	end gen_instruction;

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
					else
						-- symbol not previously seen
						declare
							sym : constant t_symbol_p := new t_symbol;
						begin
							sym.symbol_id := t_symbol_id (symbols.Length);
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
		p ("; " & t.kind'Img
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
		p ("; +expect_operator " & t.kind'Img);
		case t.kind is
			when alu_op =>
				rc := expect_expr;
				gen_instruction (kind => t.op, binary => True);
			when eclose | sclose | eot | semicolon =>
				rc := False;
			when number | eopen | sopen | error | var | set | symbol | defining_symbol | print | wloop
					| ifcond .. elsecond =>
				raise user_error with "expected an operator, got a " & t.kind'Img;
		end case;
		p ("; -expect_operator " & t.kind'Img);
		return rc;
	end expect_operator;

	function expect_expr return Boolean is
		t : constant t_token := lex;
		rc : Boolean := True;
	begin
		p ("; +expect_expr " & t.kind'Img);
		case t.kind is
			when number =>
				gen_instruction (push, t.value);
				rc := expect_operator;
			when eopen =>
				while expect_expr and then expect_operator loop
					null;
				end loop;
			when alu_op =>
				rc := expect_expr;
				gen_instruction (kind => t.op, binary => False); -- unary op
			when set | var | print | wloop | sopen | ifcond .. elsecond =>
				raise user_error with "expected an expression, got a " & t.kind'Img;
			when defining_symbol =>
				raise user_error with "symbol is undefined";
			when symbol =>
				gen_instruction (load, t.sym);
				rc := expect_operator;
			when eot | semicolon | eclose | sclose =>
				rc := False;
			when error =>
				raise user_error;
		end case;
		p ("; -expect_expr " & t.kind'Img);
		return rc;
	end expect_expr;

	function expect_stmt (stmt : t_token) return Boolean;

	function expect_ifcond return Boolean is
		footer 	: constant t_label_id := new_label;
		next	: t_label_id := new_label;
		rc 		: Boolean := True;
	begin
		rc := expect_expr;
		gen_instruction (branch_if_zero, next);
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
						gen_instruction (branch_always, footer);
						-- new test begins
						gen_instruction (label, next);
						next := new_label;
						rc := expect_expr;
						gen_instruction (branch_if_zero, next);
						-- new if block
						if not expect_stmt (lex) then
							raise user_error with "cannot end elsif block here";
						end if;
					when elsecond =>
						-- end previous if block
						gen_instruction (branch_always, footer);
						-- final if block
						gen_instruction (label, next);
						next := new_label;
						if not expect_stmt (lex) then
							raise user_error with "cannot end else block here";
						end if;
					when others =>
						-- end final if block
						gen_instruction (label, next);
						gen_instruction (label, footer);
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
		p ("; +expect_stmt " & stmt.kind'Img);
		case stmt.kind is
			when var =>
				raise user_error with "var only allowed at the beginning of the block";
			when print =>
				rc := expect_expr;
				gen_instruction (print);
			when set =>
				declare
					left : constant t_token := lex;
				begin
					expect (left, symbol);
					rc := expect_expr;
					gen_instruction (store, left.sym);
				end;
			when wloop =>
				declare
					header : constant t_label_id := new_label;
					footer : constant t_label_id := new_label;
				begin
					gen_instruction (label, header);
					rc := expect_expr;
					gen_instruction (branch_if_zero, footer);
					if not expect_stmt (lex) then
						raise user_error with "cannot end while block here";
					end if;
					gen_instruction (branch_always, header);
					gen_instruction (label, footer);
				end;
			when ifcond =>
				if not expect_ifcond then
					p ("; -expect_stmt " & stmt.kind'Img);
					return False;
				end if;
			when alu_op | defining_symbol | symbol | number | elsifcond | elsecond | eopen | eclose =>
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
								gen_instruction (push, init);
								def.sym.offset := current_offset;
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
					gen_instruction (pop);
					pop_count := pop_count - 1;
				end loop;
			when sclose =>
				p ("; -expect_stmt " & stmt.kind'Img);
				return False;
			when semicolon =>
				null;
			when eot =>
				raise end_program;
			when error =>
				raise user_error;
		end case;
		p ("; -expect_stmt " & stmt.kind'Img);
		return True;
	end expect_stmt;

begin
	p ("extern quit");
	p ("extern print");
	p ("global main");
	p ("main:");
	begin
		while expect_stmt (lex) loop
			null;
		end loop;
	exception
		when end_program =>
			null;
	end;
	if current_offset /= 0 then
		raise user_error with "stack pointer does not end at 0, it is" & current_offset'Img;
	end if;
	declare
		size : constant Natural := Natural (symbols.Length) * 4;
	begin
		p ("jmp quit");
		p ("section .data");
		p ("base:");
		p ("times" & size'Img & " db 0");
	end;
exception
	when e : user_error =>
		Ada.Text_IO.Put_Line
			(Ada.Text_IO.Standard_Error,
			 "at line" & Ada.Text_IO.Count'Image (Ada.Text_IO.Line (Ada.Text_IO.Standard_Input))
			 & " column" & Ada.Text_IO.Count'Image (Ada.Text_IO.Col (Ada.Text_IO.Standard_Input))
			 & ": " & Ada.Exceptions.Exception_Message (e));
		raise;
end expr;

