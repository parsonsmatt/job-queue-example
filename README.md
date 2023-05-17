# job-queue

A prototype repository for a means of *dynamic dispatch* of a job.

The key requirement here is that we have the ability to *send messages* - that is, serialize them, write to disk/database/redis/etc, and have another process read them.
We then want to be able to dispatch on that.

The module [`JobQueue.Sum`](src/JobQueue/Sum.hs) has a simple implementation that uses a sum type.
However, this implementation has a big cost: you need to handle/match everything in big case expressions.
As the amount of jobs in  your system grows, that cost increases.

The module [`JobQueue.Class`](src/JobQueue/Class.hs) provides an alternative. 
Instead of a single type, we represent a job as a class.
Defining a new job means defining a new datatype to represent the job.
This reduces some of the costs of defining new jobs and modifying them.
