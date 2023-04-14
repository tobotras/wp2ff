# wp2ff

Reads recent posts from Wordpress RSS feed. Posts in category "FreeFeed" are reposted to FreeFeed.

## Installation

Install Clojure and Leiningen. Clone repository from https://github.com/tobotras/wp2ff/. Run "lein uberjar",
compiled program is found at target/uberjar/wp2ff-0.1.0-SNAPSHOT-standalone.jar

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

Obvious

### Bugs

This might accidentally post a ton of junk into your FreeFeed account. Beware.

### Any Other Sections
### That You Think
### Might be Useful

## License

Copyright © 2023 Boris Tobotras, tobotras@gmail.com

This program and the accompanying materials are made available under the
terms of the GNU General Public License as published by
the Free Software Foundation, version 2 of the License.
