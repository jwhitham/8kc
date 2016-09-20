
with Ada.Text_IO;

package body backend is

	num_labels		: t_label_id := 0;
	current_offset 	: t_current_offset := 0;
	bytes_per_word	: constant Natural := 8;

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
				p ("mov rax,[rsp + (" & bytes_per_word'Img & " *" & arg & ")]");
				p ("push rax");
				current_offset := current_offset + 1;
			when store =>
				p ("pop rax");
				p ("mov [rsp + (" & bytes_per_word'Img & " * (" & arg & " - 1))], rax");
				current_offset := current_offset - 1;
			when print =>
				p ("pop rdi"); -- arg 1 (https://en.wikipedia.org/wiki/X86_calling_conventions)
				p ("call print");
				current_offset := current_offset - 1;
			when branch_if_zero =>
				p ("pop rax");
				p ("cmp rax, 0");
				p ("je " & arg);
				current_offset := current_offset - 1;
			when branch_always =>
				p ("jmp " & arg);
			when label =>
				p (arg & ":");
			when pop =>
				p ("pop rax");
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
		procedure sign_fill (r : String := "rax") is
			shift_size : constant Natural := (bytes_per_word * 8) - 1;
		begin
			p ("sar " & r & ", " & shift_size'Img); -- fill with sign bit
		end sign_fill;
		procedure eq (neq : Boolean) is
			label_id : constant t_label_id := new_label;
			text 	 : String := label_id'Img;
		begin
			text (text'First) := '_';
			p ("xor rax, rbx"); -- zero if equal
			if neq then
				p ("mov rax, -1");
			else
				p ("mov rax, 0");
			end if;
			p ("jnz B" & text);
			p ("not rax");
			p ("B" & text & ":");
		end eq;
	begin
		if not binary then
			p ("pop rax");
			case kind is
				when op_add =>
					null;
				when op_sub =>
					p ("neg rax");
				when op_lnot =>
					p ("not rax");
				when op_bnot =>
					p ("not rax");
				when others =>
					raise user_error with kind'Img & " is not a unary operation";
			end case;
			p ("push rax");
			return;
		end if;

		current_offset := current_offset - 1;
		p ("pop rbx");
		p ("pop rax");
		case kind is
			when op_add =>	p ("add rax, rbx");
			when op_sub =>	p ("sub rax, rbx");
			when op_sll =>	p ("sll rax, rbx");
			when op_srl =>	p ("srl rax, rbx");
			when op_sra =>	p ("sra rax, rbx");
			when op_mul =>	p ("imul rax, rbx");
			when op_div =>	p ("div rax, rbx");
			when op_idiv =>	p ("idiv rax, rbx");
			when op_lor =>	p ("or rax, rbx");
			when op_land =>	p ("and rax, rbx");
			when op_lxor =>	p ("xor rax, rbx");
			when op_bor =>	p ("or rax, rbx"); sign_fill;
			when op_band =>	p ("and rax, rbx"); sign_fill;
			when op_bxor =>	p ("xor rax, rbx"); sign_fill;
			when op_lt =>	p ("sub rax, rbx"); sign_fill;
			when op_gt =>	p ("sub rbx, rax"); sign_fill ("rbx"); p ("push rbx"); return;
			when op_ge =>	p ("sub rax, rbx"); p ("not rax"); sign_fill;
			when op_le =>	p ("sub rbx, rax"); p ("not rbx"); sign_fill ("rbx"); p ("push rbx"); return;
			when op_eq =>	eq (False);
			when op_ne =>	eq (True);
			when others =>
				raise user_error with kind'Img & " is not a binary operation";
		end case;
		p ("push rax");
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

