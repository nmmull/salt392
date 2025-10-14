run:
	dune exec salt -- 2 playground/src/main.rs && echo "\n\n\n"
	cd playground && cargo run
