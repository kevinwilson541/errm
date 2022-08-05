errm
=====

An OTP application for Erlang Record Relational Mapping (errm).

Dependencies
-----
    $ rebar3 get-deps

Build
-----
    $ rebar3 compile

Test
-----
    $ rebar3 eunit

Dialyzer
-----
    $ rebar3 dialyzer

Application Configuration
-----
| category               | name                         | type          | description                                                                                                                                |
|------------------------|------------------------------|---------------|--------------------------------------------------------------------------------------------------------------------------------------------|
| errm repo              | errm_default_repo_enabled    | boolean()     | Whether to enable the default repo (sets up driver, migrations, and seeds off of application env vars). Defaults to `false`.               |
| errm supervisor        | errm_sup_intensity           | pos_integer() | Supervisor intensity of default repo supervisor.                                                                                           |
| errm supervisor        | errm_sup_period              | pos_integer() | Supervisor period of default repo supervisor.                                                                                              |
| errm migrations        | errm_migration_table         | string()      | Table name to store completed migrations in default repo.                                                                                  |
| errm migrations        | errm_migration_schema        | string()      | Schema name to store completed migrations in default repo.                                                                                 |
| errm migrations        | errm_migration_path          | string()      | File path where migration files (.yaml) are stored for default repo.                                                                       |
| errm migrations        | errm_migration_shutdown      | pos_integer() | How long to wait for migrations shutdown to occur in default repo supervisor before brutal kill. Defaults to `5000`.                       |
| errm seeds             | errm_seed_table              | string()      | Table name store completed seeds in default repo.                                                                                          |
| errm seeds             | errm_seed_schema             | string()      | Schema name to store completed seeds in default repo.                                                                                      |
| errm seeds             | errm_seed_path               | string()      | File path where migration files (.yaml) are stored for default repo.                                                                       |
| errm seeds             | errm_seed_shutdown           | pos_integer() | How long to wait for seeds shutdown to occur in default repo supervisor before brutal kill. Defaults to `5000`.                            |
| errm driver management | errm_driver_module           | atom()        | Module that implements `errm_gen_driver` behavior, to be used to run database operations for default repo.                                 |
| errm driver management | errm_driver_init             | term()        | Arguments to pass to driver module's `start_link` implementation from `errm_gen_driver` for default repo.                                  |
| errm driver management | errm_driver_pool_min         | pos_integer() | Minimum number of connections to maintain against main database for default repo. Defaults to `1`.                                         |
| errm driver management | errm_driver_pool_max         | pos_integer() | Maximum number of connections to maintain against main database for default repo. Defaults to `1`.                                         |
| errm driver management | errm_driver_shutdown         | pos_integer() | How long to wait for driver shutdown to occur in default repo supervisor before brutal kill. Defaults to `5000`.                           |
| errm driver replica    | errm_driver_replica_enabled  | boolean()     | Whether to enable a pool of connections against replica database for default repo. Defaults to `false`.                                    |
| errm driver replica    | errm_driver_replica_init     | term()        | Arguments to pass to driver module's `start_link` implementation from `errm_gen_driver` to create a connection against a replica database. |
| errm driver replica    | errm_driver_replica_pool_min | pos_integer() | Minimum number of connections to maintain against replica database for default repo. Defaults to `1`.                                      |
| errm driver replica    | errm_driver_replica_pool_max | pos_integer() | Maximum number of connections to maintain against replica database for default repo. Defaults to `1`.                                      |

Status
-----

This project is currently in pre-alpha stages:
- The APIs for connection pool management and migration/seed management is more or less finalized, along with the accepted
file format. Any future additions to the migration/seed file format should be backwards compatible.
- The APIs for `errm_gen_driver` are not entirely finalized, and will evolve as more dialect differences pop up for
generalized operations (this also affects what's available on `errm_connection` as that's a wrapper `gen_server` over
the `errm_gen_driver` API). Any future additions to `errm_gen_driver` should be backwards compatible.