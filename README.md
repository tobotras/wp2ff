# wp2ff

Reads recent posts from Wordpress RSS feed. Posts in category "FreeFeed" are reposted to FreeFeed.

## Installation

Install Clojure and Leiningen. Run "lein uberjar", compiled program is found at
target/uberjar/wp2ff-0.1.0-SNAPSHOT-standalone.jar

## Usage

    $ java -jar wp2ff-0.1.0-SNAPSHOT-standalone.jar

## Environment

### Data source

DB_NAME, DB_HOST, DB_USER, DB_PASS (defaults to wp2ff/localhost/wp2ff/wp2ffpass)

### User credentials

WP_USER (WP username)  
FF_USER (FF username)  
FF_PASS (FF password)  

### Other settings

WP_SLEEP (seconds between polls)

## Examples

To deploy into GAE use app.yaml like this:

    runtime: java17
    env: standard
    env_variables:
      WP_USER: "username"
      FF_USER: "username"
      FF_PASS: "password"
      WP_SLEEP: 3600
    entrypoint: java -Xmx64m -jar wp2ff-0.1.0-SNAPSHOT-standalone.jar

## Bugs

This might accidentally post a ton of junk into your FreeFeed account. Beware.

## License

Copyright © 2023 Boris Tobotras, tobotras@gmail.com

This program and the accompanying materials are made available under the
terms of the GNU General Public License as published by
the Free Software Foundation, version 2 of the License.
