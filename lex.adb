
with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;

with backend;

package body lex is

	char 		: Character := ' ';
	text 		: Unbounded_String;
	undo_char 	: Character := Ada.Characters.Latin_1.NUL;
	symbols		: types.p_symbols.Map;

	procedure update_char is
	begin
		if undo_char /= Ada.Characters.Latin_1.NUL then
			char := undo_char;
			undo_char := Ada.Characters.Latin_1.NUL;
			return;
		end if;
		Ada.Text_IO.Get (char);
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

	function get_token return t_token is
		t : constant t_token := lex2;
	begin
		backend.comment
		   ("; " & t.kind'Img
			& " at line" & Ada.Text_IO.Count'Image (Ada.Text_IO.Line (Ada.Text_IO.Standard_Input))
			& " column" & Ada.Text_IO.Count'Image (Ada.Text_IO.Col (Ada.Text_IO.Standard_Input)));
		return t;
	end get_token;

end lex;

