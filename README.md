# Reqrep_Task_Pools

## Overview

This is an Ada 2012 package that provides a task pool system for jobs
which each take the form of a single request that receives a single
response. By limiting the communication patterns to this simplified
model, it is possible to enable multi-tasking in suitable applications
without needing to deal directly with Ada's tasking features.

## Copyright

The package is provided under the permissive ISC-style license:

> Copyright (c) 2015, James Humphry
>
> Permission to use, copy, modify, and/or distribute this software for any
> purpose with or without fee is hereby granted, provided that the above
> copyright notice and this permission notice appear in all copies.
>
> THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
> REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
> AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
> INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
> LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
> OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
> PERFORMANCE OF THIS SOFTWARE.

## Use of the `Reqrep_Task_Pools` package

The jobs to be processed by the pool must be characterised by a definite
tagged type. Values of this type should be able to describe both the
request being made, and the response received. An `Execute` function
must exist:

    function Execute
        (R : in out Reqrep) return Reqrep_Return_Status

This function must perform the necessary work, make any necessary
changes to the request-response object and then return `Success` or
`Failure` from an enumeration type `Reqrep_Return_Status` defined in
the package.

The actual task pool is then created by instantiating a generic package
within Reqrep_Task_Pools with suitable parameters:

    generic
        type Reqrep is tagged private;
        with function Execute
            (R : in out Reqrep) return Reqrep_Return_Status is <>;
        Number_Workers : Positive := 1;
    package Task_Pool

The tasks in the pool will be created automatically when the package is
instantiated. If the application is likely to be CPU-bound it would be
better to limit the number of workers to the number of CPU, but if it is
IO-bound then a larger number of worker tasks may be optimal.

The new Task_Pool package defines a new type derived from the supplied
type that contains additional information about the job status. You do
not have to create these manually as the conversion will happen
automatically when the job requests are made, but it is normally useful
to be able to query this information when the response is returned.

Requests are submitted by using `Push_Job` in the Task_Pool package.
This takes a request-response object, and a timeout which by default is
set to 60 seconds. If a job can take legitimately take longer than 60
seconds this must be increased.

Behind the scenes the job request-response objects are added to a FIFO
queue from which jobs are removed by the worker tasks. The results are
returned on a second FIFO queue, but due to non-deterministic
scheduling, there is usually no guarantee that the order will be
preserved, even if the jobs are all known to take the same amount of
CPU time. This is why the request data and response data are always
kept together.

After the work is performed, request-response objects can be retrieved
by using `Get_Result`. There are two possible return types - either the
type specified by the user or the extended derived version. If the
extended version is retrieved it is possible to find out the status of
the response using the `Status` function.

The worker tasks will not automatically shut down when the main task
ends. It is therefore necessary to call the `Shutdown` procedure to
make sure they are all terminated.

## Error handling

Apart from `Success` or `Failure`, it is also possible to see
the `Timeout` status if the run-time of the `Execute` function exceeds
the limit set. Remember that the request-response object may be in an
inconsistent state in this case.

If an exception arose that was not caught and dealt with within the
function, the status will be set to `Unhandled_Exception`. A copy of
the exception occurrence is saved and the response queue is frozen so
that it is clear which request-response the saved occurrence relates
to. The procedure `Get_Exception` can be used to retrieve the exception
occurrence for logging or display, and it will also unfreeze the
response queue.

The procedure `Discard_Exception` can be used to unfreeze the response
queue if there is nothing useful that can be done with the exception
information.

As a result of the need to freeze the response queue, unhandled
exceptions may significantly damage performance. It is recommended that
exceptions should only be propagated where they are truly
unpredictable. For example, if the task pool is being used to retrieve
a data from resources on the internet, a network connectivity failure
is not really unpredictable and the request-response objects should be
able to indicate this without raising an exception.

## Examples

Two examples are provided, `simple_example.adb` which illustrates the
basic mechanism, and `error_handling_example.adb` which demonstrates the
error handling mechanisms discussed above.

