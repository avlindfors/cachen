# cachen

Simple *POC* for an LRU web cache, implemented in Erlang. 

## cachen?

`Cachen` is a slangy translation of *the cache* to Swedish.

## Dependencies 

The application depends on 
 * [`Cowboy`](https://github.com/ninenines/cowboy), en Erlang HTTP server
 * [`erlang-lru`](https://github.com/barrel-db/erlang-lru), a simple LRU cache
 * [`rebar3`](https://www.rebar3.org/) is needed to build the application
 
## Usage
There is a simple Makefile to hide and simplify rebar3 usage.
#### Build release
Build release and run it:
```bash
make rel
```
This will also start the Cowboy HTTP server and the lru cache. 
All requests on localhost will go through the cache before returning the response. All requests currently go through the same handler; a simple hello world example.
#### Start release
If a release has previously been built the following can be used to start it:
```bash
make start
```

#### Compilation
If you only want to compile the application, use:
```bash
make compile 
```
or simply
```bash
rebar3 compile
```

#### Examples 

* Watch the terminal window running `cachen` 
* Go to `localhost:8080` in your browser 
* There will be two requests, one for the resource at path `"/"` and one for `"/favicon.ico"`
* The cache has not seen the resources before so the responses will be cached before they are returned
    * The cache now holds 2 entries
* Update the page, this will result in a cache hit for `"/"` and the response is fetched from cache storage
    * The cache still holds 2 entries
* In the same session, go to `localhost:8080/test`
* This will cause a cache miss for the resource at `"/test"` and a hit for `"/favicon.ico"`
    * The cache now holds 3 entries
 
## Important notes 
* v 0.0.1
    * Nothing is persistent
    * The default cache size is 4, for easy debugging and testing
