salt0:
	dune exec salt -- 0 playground/src/main.rs && echo "\n\n\n"

	cd playground && cargo run

salt1:
	dune exec salt -- 1 playground/src/main.rs && echo "\n\n\n"
	cd playground && cargo run

salt2:
	dune exec salt -- 2 playground/src/main.rs && echo "\n\n\n"
	cd playground && cargo run

clean:
	dune clean
	cd playground && cargo clean
