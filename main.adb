
with backend;
with grammar;

procedure main is
begin
	backend.init;
	grammar.parse;
	backend.fini;
end main;
