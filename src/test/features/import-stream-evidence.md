# Streamed import: drain heartbeats before the terminal result

## Scope

`STREAM-IMPORT` owns a background heartbeat producer. A successful stream must
publish progress and heartbeat comments while RUN works, stop and join that
producer, publish one terminal result, and close the writer. RUN/progress-writer
failures must still join the producer and close, without a success result.
This change does not alter append performance, store transactions, Git fixtures,
or the import scenario registry.

The successful path signals and joins before publishing the result. Unconditional
cleanup repeats that operation so failure paths retain their ownership guarantee.
Joining happens outside SEND's lock; otherwise a producer already in SEND could
block the join indefinitely. A writer that never returns can still block draining:
this is an ordering guarantee, not bounded network-write cancellation.

## Regression design

`A-STREAMED-IMPORT-PINGS-WHILE-IT-WORKS` no longer infers scheduling from a sleep or
requires a minimum number of heartbeats in a fixed time window. A semaphore holds
the first heartbeat inside the writer. RUN waits until that heartbeat has entered,
releases it, and returns. The result writer records whether that captured producer
thread is still alive. It must already have been joined. The test also checks a
progress item, an actual heartbeat, the terminal result and summary, producer
termination on return, and no writes after close. Five-second waits bound broken
fixture setup only; their duration is not evidence of the ordering.

The old implementation leaves the producer alive until cleanup after result
publication, so the producer-quiescence assertion fails regardless of whether the
OS happens to schedule another heartbeat during the result write. This negative
control tests the prerequisite that prevents heartbeat-after-result, rather than
trying to win a timing race to observe that symptom.

`A-FAILED-STREAMED-IMPORT-STOPS-ITS-HEARTBEAT` waits for a real heartbeat, then
injects RUN and progress-writer failures separately. Both must preserve the
original error, join the producer, close exactly once, publish no result, and
perform no writes after close.

Both regressions are registered in the ordinary `nodecode/test` suite. They need
no network or account credentials. Focused fresh validation and the temporary
old-runtime negative control are recorded in the external PR preparation report;
no full-suite-green claim follows from these focused tests.
