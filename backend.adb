
with Ada.Text_IO;

package body backend is

	num_labels		: t_label_id := 0;
	current_offset 	: t_current_offset := 0;

	function new_label return t_label_id is
		l : constant t_label_id := num_labels;
	begin
		num_labels := num_labels + 1;
		return l;
	end new_label;

	procedure p (s : String) renames Ada.Text_IO.Put_Line;
	procedure comment (s : String) renames p;

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
			when func_begin =>
				p ("global " & arg);
				p (arg & ":");
			when func_end =>
				p ("ret");
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

	function get_current_offset return t_current_offset is
	begin
		return current_offset;
	end get_current_offset;

	procedure init is
	begin
		p ("extern quit");
		p ("extern print");
		p ("section .text");
	end init;

	procedure fini is
	begin
		p ("jmp quit");
		if current_offset /= 0 then
			raise user_error with "stack pointer does not end at 0, it is" & current_offset'Img;
		end if;
	end fini;

end backend;

