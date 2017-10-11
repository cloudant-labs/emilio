Exports should be grouped correctly

The groups of exports should follow the order of public API, behavior
callbacks, and private module callbacks. Each behavior should have its
own export group to delineate callbacks associated with each behavior.
This has the benefit of indicating the purpose of each exported function.
