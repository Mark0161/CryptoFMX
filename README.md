# CryptoFMX
### Sample Delphi Programming - REST/PPL/FMX using www.coingecko.com api.
<BR/>
<BR/>

![Alt text](Assets/FMXCrypto2Small.png "a title")

#### USES
* Delphi Community edition 10.4 with FMX framework. Download at [Delphi Community edition ](https://www.embarcadero.com/products/delphi/starter/free-download/)
* REST/JSON -> [CoinGecko API]('https://api.coingecko.com/api/v3/') to obtain crypto details.
* [DEB Event Bus framework for Delphi](https://github.com/spinettaro/delphi-event-bus) 
* TeeChart Components, comes with Delphi Community edition.

### References 
* [Parallel Programming Library](https://docwiki.embarcadero.com/RADStudio/Sydney/en/Using_the_Parallel_Programming_Library)
* [Delphi-event-bus](https://github.com/spinettaro/delphi-event-bus)
* [Managed Records](https://docwiki.embarcadero.com/RADStudio/Sydney/en/Custom_Managed_Records)
* [Managed Records articles ](https://blog.grijjy.com/2020/08/03/automate-restorable-operations-with-custom-managed-records/)
* [System.TMonitor](https://docwiki.embarcadero.com/Libraries/Sydney/en/System.TMonitor)
* [Delphi: Wait for threads to finish](https://stackoverflow.com/questions/33345396/delphi-wait-for-threads-to-finish)

### Few Hightlights
* TTasks/PPL for _asynchronous tasks_, reducing impacting on main UI thread.
* Event bus used to coordinate results of _asynchronous tasks_ with main UI, both for
    * User initiated events 
    * Timer events which polls for periodic updates.
* Interfaces used to separate concerns and reflect state.
* Managed Records using assigned to copy data
* TMonitor used as a precaution.
* Functions that return futures.
* _nActiveTasks_ keeps a count of async tasks running. Prevents Application from Quiting if any tasks active. _ie stop a TTask outliving the application_. 

### Status 
    Pending a final tidy up.


