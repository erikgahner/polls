Data on public support for political parties in Denmark, 2010-
---

### Description

Collection of opinion polls conducted by _Megafon_, _Gallup_, _Greens_, _Rambøll_, _YouGov_, _Voxmeter_, _Epinion_, _Norstat_ and _Wilke_ for the period 2010-.

### Repository content

The `polls.csv` data file consists of opinion polls on the support for nine political parties, including all parties represented in the Danish parliament as of 2011. Each row in the dataset is an opinion poll with the following information:

 - `id` = ID for each polls (note: this is _not_ a unique ID)
 - `pollingfirm` = Polling firm conducting the poll
 - `year` = Year of end of data collection
 - `month` = Month of end of data collection
 - `day` = Day of end of data collection
 - `party_a` = Support for _Socialdemokraterne_ (in %)
 - `party_b` = Support for _Det Radikale Venstre_ (in %)
 - `party_c` = Support for _Konservative_ (in %)
 - `party_d` = Support for _Nye Borgerlige_ (in %)
 - `party_e` = Support for _Klaus Riskær Pedersen_ (in %)
 - `party_f` = Support for _SF_ (in %)
 - `party_g` = Support for _Veganerpartiet_ (in %)
 - `party_i` = Support for _Liberal Alliance_ (in %)
 - `party_k` = Support for _Kristendemokraterne_ (in %) (note: limited data)
 - `party_o` = Support for _Dansk Folkeparti_ (in %)
 - `party_p` = Support for _Stram Kurs_ (in %)
 - `party_v` = Support for _Venstre_ (in %)
 - `party_oe`= Support for _Enhedslisten_ (in %)
 - `party_aa` = Support for _Alternativet_ (in %)
 - `n` = Sample size
 - `source` = URL to source (note: limited data)

The `polls.R` file shows how one can access the data from [R](http://www.r-project.org/) and create a simple plot with the trend for _Venstre_ from the 2011 election till today.

### Data sources

 - [Voxmeter](voxmeter.dk/index.php/meningsmalinger/)
 - [Berlingske Barometer](https://www.berlingske.dk/barometeret)
 - [Ritzau Index](https://www.ritzau.dk/Produkter%20og%20Services/Ritzau%20Index.aspx)
 - [Danish Polls](https://github.com/ndarville/danish-polls) (by [@ndarville](https://github.com/ndarville))
 - [Wikipedia: Opinion polling for the Danish general election, 2015](http://en.wikipedia.org/wiki/Opinion_polling_for_the_Danish_general_election,_2015)
 - [TV2/Politik](http://politik.tv2.dk/)
 - [DR](http://dr.dk)
 - [MetroXpress](http://www.mx.dk/)
 - [Jyllands-Posten](http://jyllands-posten.dk/)
 - [Børsen](http://borsen.dk)
