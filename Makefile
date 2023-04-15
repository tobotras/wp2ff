jar = target/uberjar/wp2ff-0.1.0-SNAPSHOT-standalone.jar

JAR: $(jar)

$(jar): project.clj test/su/msk/xtalk/wp2ff/*.clj resources/log4j.properties src/su/msk/xtalk/wp2ff/*.clj
	lein uberjar

deploy: $(jar)
	cp $(jar) .
	gcloud app deploy app.yaml
	gcloud app deploy cron.yaml
