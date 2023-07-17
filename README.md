# STM Variants

I really like the `stm` API, and I would like to use it for more projects. I am thinking about creating `stm` variants which improve upon the `stm` library's offering by supporting these increasingly-difficult properties:

## Durable

`stm`'s `TVar`s are ephemeral: once the program stops, the values are lost. I would like to be able to make atomic changes to the program's configuration and to see those changes next time I start the program. Or to allow the user to open a project file, atomically modify the project via a bunch of `TVar`s, and allow the user to open that project file in a later session and see their changes.

I am thinking about the following API:

```haskell
withStore
  :: FilePath
  -> (Store -> IO a)  -- bracketed to save the DVars when unwinding
  -> IO a

-- for now; see the last section
type DTM a = STM a

-- When reloading after a crash, the DVars which use the same Store are
-- guaranteed to have seen the same transactions
atomically
  :: DTM a
  -> IO a

-- Throws an exception if a DVar exists in the Store but its serialization
-- doesn't match the given Serializable instance, as can happen if the program
-- implementation has changed or if the name is accidentally reused for
-- variables of different types.
newDVar
  :: Serializable a
  => Store
  -> String  -- a unique name
  -> a  -- the value to use if the DVar is not yet in the Store
  -> DTM (DVar a)

readDVar
  :: Serializable a
  => DVar a
  -> DTM a

writeDVar
  :: Serializable a
  => DVar a
  -> a
  -> DTM ()
```

Maybe with more parameters, to allow different storage options, e.g. a file or a database? And to allow more structured names than just `String`.


## Large

`stm`'s `TVar`s are stored in memory. This limits `stm`'s applicability to workloads which fit comfortably fits in memory. The obvious solution is to use a database, but I don't want to manipulate my data using SQL, I want to use `stm`'s nice API. Can we store the `TVar`s on disk?

I am thinking about using the same API as in the previous section, but with a more complicated implementation where `DVar`s get serialized to disk when they get garbage-collected, and then they can be reloaded again via `newDVar`. Tree structures would typically hold the name of the sub-`DVar`s rather than the `DVar`s themselves, so that they only get loaded if needed.

Or maybe they should hold an `LDVar a` (for Lazy DVar), whose more precise type indicates the type of the `DVar`, and which, once forced, would keep the `DVar` in memory until the `LDVar` gets garbage-collected as well. This would make those tree structures a lot more ergonomic.

It would also make a lot of sense to define an `DMap` type, which would offer a `Map`-like API in which the keys and values are stored on disk, and would manage the naming of the underlying `DVar`s. There should probably be two variants: `DMap` would give you a `DVar` which goes back to disk once it gets garbage-collected, useful for a top-level map of all the objects in the system, while `LDMap` gives you a `DVar` which stays in memory as long as the `LDMap` does, useful as a field inside a tree structure which will get garbage collected in due time.

Note that in the previous section, each program invocation was only intended to call `newDVar` once on each key. In this section, `newDVar` is now intended to be called often on the same key. Thus the implementation must be careful to use the same underlying `TVar` (if the `stm` implementation is used under the hood), so that changes to one `DVar` wake up transactions blocked on the `DVars` obtained from the same name.


## Distributed

The other reason to use a database is when multiple processes, possibly on different machines, access the same data. Again, I would rather use `stm`'s nice API than SQL. Can I store `TVar`s in a database?

Up to now, it was possible to serialize the `DVar`s asynchronously, after the `atomically` block has finished. But now, we need to write the new values to the database right away, so we don't get non-serializable situations in which two machines read the old value of a `DVar` and each overwrites it with its own unique value. API-wise, this requires the library to own the `DTM` type, we can't simply reuse `STM`.

One nice thing about the multi-Store design is that each machine can store its own `DVar`s locally, and `atomically`'s transactional semantics are already quite clear on what is guaranteed in case of a crash. If we want, we can allow regular `TVar` to mix with `DVar`s in the same transaction, with the semantics that in case of a crash, the `TVar`s are lost.

Another difference is that the Store must now subscribe to the database changes for the `DVar`s which are loaded in memory, so that changes on one machine wake up the other machines which are blocked on the changed `DVar`s.

This section is the most powerful of the three, in the sense that if we implement only this API, we can use this API in the other two simpler settings. But it would probably still make sense to define three separate APIs:
1. a variant with a simple API which is basically `stm` plus a Serializable constraint.
2. a variant with a slightly more complex API where users need to think about whether they want to use a `DVar` or an `LDVar`, and must be careful about not holding onto those vars for longer than needed.
3. a variant with basically the same slightly-more-complex API, except it is no longer compatible with other `stm` libraries, and `atomically` blocks take longer to commit, so you should only pay for this if you need it.


# Other variants

In a completely different direction, one limitation of STM is that it can live-lock (or be very slow) when there is a lot of contention. This is because it uses optimistic concurrency control, which needs to rollback and retry quite a lot in that case. It might be nice to keep the same API, but offer an implementation which makes a different tradeoff in its implementation, e.g. by numbering and locking each TVar. To avoid deadlocks, if a transaction locks TVar number 2 and then later tries to lock TVar number 1, the transaction would release the locks, retry, except this time the transaction begins by locking TVar number 1 and then TVar number 2.
