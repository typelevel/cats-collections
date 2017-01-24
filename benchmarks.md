=== Append

DList and Dequeue are both structures which are attractive in that the
allow for fast append operations. Whereas `append` on the more
famiiliar List structure is O(n) time, it is a constant time operation
with these structures. This benchmark is just appending bundles of 10
elements, n times. 

The results are measured in operations per second. Larger numbers are
better. Some observations

- The List list performs horribly, as expected.

- The dequeue in this case works just like prepending to a list, it is fast.

- The DList is much faster. There will be downsides to the DList that
  will be shown later when you want to get elements out of a DList,
  but if the part of the code you are optimizing for is the append,
  then this is a good choice. This makes DLists a particularly good
  choice for accumulating logs in a Writer monad.

    |-------+---------------+----------------+----------------+---------------|
    |     n | scala List    | dogs Dequeue   | dogs DList     | scalaz DList  |
    |-------+---------------+----------------+----------------+---------------|
    |    10 | 701.1M ± 3.6% | 3.8M ± 14.5%   | 121.6M ± 12.2% | 37.2M ± 23.5% |
    |   100 | 72.3K ± 30.6% | 363.4K ± 31.8% | 15.1M ± 14.6%  | 9.1M ± 15.0%  |
    |  1000 | 632 ± 3.9%    | 53.8K ± 17.6%  | 1.5M ± 14.6%   | 1.0M ± 20.3%  |
    | 10000 | 5 ± 0.0%      | 6.6K ± 33.6%   | 125.5K ± 15.5% | 83.8K ± 18.4% |
    |-------+---------------+----------------+----------------+---------------|

=== AppendThenIterate

This benchmark tries to show that when it comes to getting elements
back out of a DList, its advantage goes away. This benchmark is the
same as the above append benchmark, but after we append all the
elements, we then iterate through all of the elements from head to
tail, and now the advantage that DList had above has been erased.

    |-------+---------------+----------------+----------------|
    |     n | dogs List     | dogs Dequeue   | dogs DList     |
    |-------+---------------+----------------+----------------|
    |    10 | 66.1M ± 3.2%  | 2.0M ± 10.4%   | 2.1M ± 28.8%   |
    |   100 | 71.5K ± 10.8% | 217.4K ± 32.8% | 176.4K ± 12.6% |
    |  1000 | 629 ± 8.5%    | 23.3K ± 59.3%  | 22.1K ± 2.1%   |
    | 10000 | 5 ± 0.0%      | 2.3K ± 26.1%   | 2.1K ± 15.2%   |
    |-------+---------------+----------------+----------------|

=== DietAdd

    |-------+-----------------+--------------------+------------------+----------------------|
    |     n | Dogs Diet add 1 | dogs Diet add many | scalz Diev add 1 | scalaz Diev add many |
    |-------+-----------------+--------------------+------------------+----------------------|
    |    10 | 3.1M ± 7.5%     | 54.7M ± 10.5%      | 430.2K ± 4.5%    | 40.2M ± 17.9%        |
    |   100 | 327.7K ± 6.3%   | 2.3M ± 3.6%        | 41.2K ± 8.3%     | 550.4K ± 7.5%        |
    |  1000 | 25.5K ± 19.0%   | 198.5K ± 7.4%      | 3.8K ± 14.3%     | 47.5K ± 6.3%         |
    | 10000 | 2.6K ± 7.0%     | 19.3K ± 33.0%      | 397 ± 9.8%       | 4.7K ± 13.6%         |
    |-------+-----------------+--------------------+------------------+----------------------|

=== DietSearch

    |---------------+---------------|
    | dogs Diet     | scalaz Diev   |
    |---------------+---------------|
    | 126.9K ± 5.8% | 38.6K ± 38.0% |
    |---------------+---------------|

===  DietRandomize

    |-------+-----------------+--------------------+-------------------+----------------------|
    |     n | Dogs Diet Add 1 | dogs Diet add many | scalaz Diev add 1 | scalaz Diev add many |
    |-------+-----------------+--------------------+-------------------+----------------------|
    |    10 | 549.0K ± 7.0%   | 658.2K ± 7.2%      | 205.4K ± 6.3%     | 293.9K ± 11.0%       |
    |   100 | 40.2K ± 7.0%    | 57.1K ± 10.4%      | 11.5K ± 9.0%      | 26.9K ± 6.4%         |
    |  1000 | 2.4K ± 28.8%    | 1.3K ± 9.3%        | 439 ± 6.6%        | 1.7K ± 25.6%         |
    | 10000 | 164 ± 24.3%     | 1 ± 500.0%         | 5 ± 0.0%          | 77 ± 7.7%            |
    |-------+-----------------+--------------------+-------------------+----------------------|

===  List Filter

    |-------+----------------+---------------+----------------|
    |     n | Dogs List      | scala List    | scalaz IList   |
    |-------+----------------+---------------+----------------|
    |    10 | 11.3M ± 7.1%   | 5.2M ± 5.9%   | 5.6M ± 7.6%    |
    |   100 | 1.2M ± 20.6%   | 553.6K ± 5.8% | 636.3K ± 16.4% |
    |  1000 | 125.9K ± 18.3% | 56.5K ± 13.1% | 56.4K ± 21.2%  |
    | 10000 | 13.7K ± 71.5%  | 7.6K ± 227.8% | 5.0K ± 38.1%   |
    |-------+----------------+---------------+----------------|

=== List flatMap

    |-------+---------------+----------------+---------------|
    |     n | Dogs List     | scala List     | scalaz IList  |
    |-------+---------------+----------------+---------------|
    |    10 | 6.6M ± 14.8%  | 2.4M ± 30.1%   | 2.6M ± 9.1%   |
    |   100 | 648.8K ± 7.4% | 265.0K ± 24.9% | 284.7K ± 7.1% |
    |  1000 | 67.0K ± 7.9%  | 24.1K ± 36.3%  | 25.1K ± 35.0% |
    | 10000 | 7.9K ± 25.3%  | 13.3K ± 66.6%  | 2.4K ± 26.0%  |
    |-------+---------------+----------------+---------------|

=== List foldLeft

    |-------+----------------+---------------+---------------|
    |     n | Dogs List      | scala List    | scalaz IList  |
    |-------+----------------+---------------+---------------|
    |    10 | 26.3M ± 20.8%  | 26.0M ± 6.4%  | 26.0M ± 29.3% |
    |   100 | 2.2M ± 18.9%   | 2.2M ± 23.7%  | 2.2M ± 20.7%  |
    |  1000 | 226.5K ± 11.7% | 220.8K ± 4.8% | 226.3K ± 7.7% |
    | 10000 | 19.7K ± 26.9%  | 17.8K ± 32.2% | 19.8K ± 35.1% |
    |-------+----------------+---------------+---------------|

=== List map

    |-------+---------------+---------------+---------------|
    |     n | Dogs List     | scala List    | scalaz IList  |
    |-------+---------------+---------------+---------------|
    |    10 | 2.4M ± 16.7%  | 13.3M ± 15.7% | 4.5M ± 11.1%  |
    |   100 | 233.6K ± 5.3% | 1.3M ± 24.8%  | 454.6K ± 9.4% |
    |  1000 | 20.7K ± 24.8% | 82.3K ± 15.4% | 43.5K ± 23.1% |
    | 10000 | 1.7K ± 25.0%  | 10.5K ± 17.3% | 3.3K ± 29.9%  |
    |-------+---------------+---------------+---------------|
