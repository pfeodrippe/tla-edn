clj_cmd = env clj

.PHONY: build
build:
	mkdir -p target
	$(clj_cmd) -X:depstar uberjar :jar target/tla-edn.jar :sync-pom true :version '"0.32.0"' :exclude '["clojure/.*", "dynapath/.*", "babashka.*" "clojure.java.classpath/.*"]' :compile-ns '[tla-edn.spec]'

.PHONY: deploy
deploy:
	mvn deploy:deploy-file -Dfile=target/tla-edn.jar -DpomFile=pom.xml -DrepositoryId=clojars -Durl=https://clojars.org/repo/

.PHONY: test
test:
	$(clj_cmd) -A:test

.PHONY: autotest
autotest:
	$(clj_cmd) -A:test --watch
