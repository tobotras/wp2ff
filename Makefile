jar = target/uberjar/wp2ff-0.1.0-SNAPSHOT-standalone.jar

JAR: $(jar)

$(jar): project.clj test/su/msk/xtalk/wp2ff/*.clj resources/log4j.properties src/su/msk/xtalk/wp2ff/*.clj
	lein uberjar

.PHONY: deploy test run _check-make-vars-defined

Makefile: _check-make-vars-defined

_check-make-vars-defined:
ifndef FF_PASS
    $(error FF_PASS is not set)
endif
ifndef WP_PASS
    $(error WP_PASS is not set)
endif

deploy: $(jar)
	cp $(jar) .
	yes | (gcloud app deploy app.yaml; gcloud app deploy cron.yaml)

test:
	PORT=8090 WP_USER=tobotras FF_USER=tobotras lein with-profile test test

run:
	PORT=8090 WP_USER=tobotras FF_USER=tobotras lein run
