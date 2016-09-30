# mirage-event-clock

A Mirage MCLOCK/TIME implementation that only moves forward during sleep. This library is mainly intended for faster testing of other Mirage libraries that depend on a MCLOCK and/or TIME module, such as network protocols.

This library is based on an original idea by Thomas Leonard used in the unit tests in [Cuekeeper](https://github.com/talex5/cuekeeper). A library implementation of Thomas' clock is available in [mirage-clock-test](https://github.com/talex5/mirage-clock-test).
