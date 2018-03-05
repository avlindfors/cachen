# cachen

Simple *POC* for an LRU web cache, implemented in Erlang. 

## cachen?

`Cachen` is a slangy Swedish translation of *the cache*.

## Dependencies 

The application depends on 
 * [`Cowboy`](https://github.com/ninenines/cowboy), en Erlang HTTP server
 * [`erlang-lru`](https://github.com/barrel-db/erlang-lru), a simple LRU cache
 * [`rebar3`](https://www.rebar3.org/) is needed to build the application
 
## Usage
#### Clone repo
Clone the github repo in your location of choice:
```bash
$ git clone https://github.com/alindfor/cachen.git
$ cd cachen
```

#### Build release
There is a simple Makefile to hide and simplify rebar3 usage.<br>

Build release and run it:
```bash
$ make rel
```
This will also start the Cowboy HTTP server and the lru cache. 
All requests on localhost will go through the cache before returning the response. All requests currently go through the same handler; a simple hello world example.
#### Start release
If a release has previously been built the following can be used to start it:
```bash
$ make start
```

#### Compilation
If you only want to compile the application, use:
```bash
$ make compile 
```
or simply
```bash
$ rebar3 compile
```

## Examples 

* Watch the erl shell running `cachen` 
* Go to `localhost:8080` in your browser 
* There will be two requests, one for the resource at path `"/"` and one for `"/favicon.ico"`
* The cache has not seen the resources before so the responses will be cached before they are returned
    * The cache now holds 2 entries (`cachen:size()` in the erl shell)
* Update the page, this will result in a cache hit for `"/"` and the response is fetched from cache storage
    * The cache still holds 2 entries
* In the same session, go to `localhost:8080/test`
* This will cause a cache miss for the resource at `"/test"` and a hit for `"/favicon.ico"`
    * The cache now holds 3 entries

## Functions
The most important functions are listed below:
* `set_max_size/1`:
    * Set the maximum size of the cache
* `max_size/0`:
    * Returns the maximum size of the cache
* `size/0`:
    * Returns the current number of entries in the cache
* `items/0`:
    * Returns a list of tuples representing the key-value pairs in the cache

**Most functions are exported at the moment to facilitate development and testing**
## Important notes 
* v 0.0.1
    * Nothing is persistent
    * The default cache size is 4, for easy debugging and testing
        * This can be changed at runtime by using the function `cachen:set_max_size/1` or in code in `cachen_sup.erl`
