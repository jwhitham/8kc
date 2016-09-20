
with types; use types;

package backend is

	function new_label return t_label_id;
	procedure gen_instruction (kind : t_instruction_kind; arg : String := "");
	procedure gen_instruction (kind : t_instruction_kind; label_id : t_label_id);
	procedure gen_instruction (kind : t_alu_op_kind; binary : Boolean);
	procedure gen_instruction (kind : t_instruction_kind; value : t_value);
	procedure gen_instruction (kind : t_instruction_kind; sym : t_symbol_p);
	procedure comment (s : String);
	procedure init;
	procedure fini;
	function get_current_offset return t_current_offset;

end backend;

