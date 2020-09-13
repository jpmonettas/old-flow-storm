.PHONY: install deploy clean help

clean:
	-rm flow-storm.jar
	# -rm pom.xml
	-rm target -rf

flow-storm.jar:
	clj -A:jar flow-storm.jar

pom.xml:
	clj -Spom
	mvn versions:set -DnewVersion=$(version)

make-cljs-tests:
	npm i
	npx shadow-cljs compile tests

run-cljs-tests: make-cljs-tests
	node output/run-tests.js

run-clj-tests:
	clj -A:test

run-tests: run-cljs-tests run-clj-tests

release: flow-storm.jar pom.xml

install: flow-storm.jar pom.xml
	mvn install:install-file -Dfile=flow-storm.jar -DpomFile=pom.xml

deploy:
	mvn deploy:deploy-file -Dfile=flow-storm.jar -DrepositoryId=clojars -DpomFile=pom.xml -Durl=https://clojars.org/repo

tag-release:
	git add CHANGELOG.md && \
	git commit -m "Updating CHANGELOG after $(version) release" && \
	git tag "v$(version)" && \
	git push origin master

help:
	@echo "For releasing to clojars run"
	@echo "make version=x.y.z release"
