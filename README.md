Erlang Rackspace Cloudfiles API
===============================

This Erlang application allow you to interface with the Rackspace Cloudfiles API using a very simple set of functions. The API can be access manually by using the provided modules, or through a gen_sever that maintains the approperate state information between API calls.

Compilation
-----------

This application make use of rebar for compilation, if you have a copy of [rebar](https://github.com/basho/rebar) avaliable standard compilation semantics apply. If not, there is a included script that will build a copy of rebar.

    ./makerebar.sh
    ./rebar get-deps
    ./rebar compile

There is a dependency on [ibrowse](https://github.com/basho/rebar) which is used for the interaction with Cloudfile's REST interface.

Usage
-----

Usage of this application is provided through a simple interface, however there are two modes of operation. The first is through a gen_server that maintains the state of the Cloudfile connection (the Authentication Token, Management URLs, etc.). The second is by maintaining the state yourself (through a record structure that is passed to every function).

The following example shows the interaction with this application:

Starting the Application, this will start all the required services:

```erlang
1> rackspace_cloud_files:start().
ok
```

Obtain an Authentication Token that must be used for all future calls. You can authenticate against ```us```, ```uk```, or you can specify your own URL:

```erlang
2> rackspace_cloud_files:get_auth_token(uk, Username, APIKey).
ok
```

Once authenticated you can work with containers:

```erlang
3> rackspace_cloud_files:create_container("test1").
ok
4> rackspace_cloud_files:create_container("test2").
ok
5> rackspace_cloud_files:list_containers().
["test1","test2"]
6> rackspace_cloud_files:delete_container("test1").
ok
7> rackspace_cloud_files:list_containers().
["test2"]
```


You can interact with objects in the containers:

```erlang
10> rackspace_cloud_files:list_objects("test2").
[]
11> rackspace_cloud_files:create_object("test2", "newObject.txt", <<"hello world">>).             
ok
12> rackspace_cloud_files:create_object("test2", "newObject2.txt", <<"hello world2">>).
ok
13> rackspace_cloud_files:list_objects("test2").
["newObject.txt","newObject2.txt"]
14> rackspace_cloud_files:delete_object("test2", "newObject.txt").
ok
15> rackspace_cloud_files:list_objects("test2").                                       
["newObject2.txt"]
16> rackspace_cloud_files:retrieve_object_metadata("test2", "newObject2.txt").
[{"Content-Length","12"},
 {"Content-Type","text/plain"},
 {"Last-Modified","Sat, 25 Aug 2012 17:41:46 GMT"},
 {"Hash","cc2c857f89648dbd139d7b2a6665957d"}]
17> rackspace_cloud_files:get_object("test2", "newObject2.txt").
<<"hello world2">>
18> rackspace_cloud_files:get_object("test2", "newObject2.txt", "/tmp/newFile.txt").
ok
```

Other operations that are supported include:

* Modifying metadata for containers and objects
* Retrieving account metadata
* Uploading files to containers
* Copying and moving objects
* Use of the ```delete_after``` and ```delete_at``` options for objects
* Sending compressed objects (gzip)
* CDN operations

License
-------

This application is licensed under an [Apache 2.0 License](http://www.apache.org/licenses/LICENSE-2.0.html)

    Copyright 2012 David Ellefsen

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

Disclamer
---------

This application should be considered in a beta state of development, should it eat your files, be it on your own head.

TODO
----

* Implement object versions
* Create simpler interface for static web pages (although this can be achieved using the provided functions)
* Improve documentation
