
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;

package types is

	type t_alu_op_kind is (op_add, op_sub, op_bor, op_band, op_bxor, op_sll, op_srl,
						op_sra, op_lt, op_le, op_gt, op_ge, op_eq, op_ne, op_mul,
						op_idiv, op_div, op_land, op_lxor, op_lor, op_lnot, op_bnot);
	type t_token_kind is (eopen, eclose, sopen, sclose, error, number, alu_op, print, wloop,
						semicolon, eot, symbol, set, var, defining_symbol,
						ifcond, elsifcond, elsecond, func);
	type t_instruction_kind is (push, unary_op, binary_op, load, store, print,
						branch_if_zero, branch_always, label, pop, func_begin, func_end);
	type t_value is new Natural;
	type t_symbol_id is new Natural;
	type t_label_id is new Natural;
	type t_current_offset is new Natural;

	type t_symbol is record
		symbol_id 	: t_symbol_id := 0;
		offset    	: t_current_offset := 0;
		has_body	: Boolean := False;
		name		: Unbounded_String;
	end record;
	type t_symbol_p is access t_symbol;

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

	package p_symbols is new Ada.Containers.Hashed_Maps
		(Unbounded_String, t_symbol_p, Ada.Strings.Unbounded.Hash, "=", "=");

	user_error	: exception;

end types;

