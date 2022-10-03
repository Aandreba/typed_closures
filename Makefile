test:
	cd tests && cargo run

test_doc:
	cd tests && cargo doc --open

test_expand:
	cd tests && cargo expand