jar = target/uberjar/wp2ff-0.1.0-SNAPSHOT-standalone.jar

$(jar): project.clj test/su/msk/xtalk/wp2ff/core_test.clj resources/log4j.properties src/su/msk/xtalk/wp2ff/core.clj
	lein uberjar

deploy: $(jar)
	cp $(jar) .
	gcloud app deploy app.yaml --verbosity=info
