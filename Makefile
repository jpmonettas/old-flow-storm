.PHONY: 

lint:
	clj-kondo --config .clj-kondo/config.edn --lint src
