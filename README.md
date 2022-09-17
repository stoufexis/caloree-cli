# Caloree CLI

Caloree is a simple, self hosted, cli based nutrition tracker.

**Related repositories:**
* [Database](https://github.com/StefanosTouf/caloree-database)
* [Server](https://github.com/StefanosTouf/caloree-server)

## Setting up
* Install the server by following the instructions [here](https://github.com/StefanosTouf/caloree-server/blob/master/README.md). The server will be unavailable for a couple of minutes after the first time it's started up, due to the required static data (foods and nutrients) being added to the database.
* Install the cli:
  * Clone this repository `git clone git@github.com:StefanosTouf/caloree-cli.git`
  * Run `stack install .` at the root of the project
  * The `caloree` command should now be available

## Note
* The foods that are available on startup along with information about their nutrients have been collected from the [USDA FoodData Central](https://fdc.nal.usda.gov/)

## Caveats
* Secure communication through HTTPS is technically supported, but not implemented yet. Thus, the server should either be installed locally or accessed via other secure communication channels (e.g. ssh tunelling).
* Around 17.000 foods are available on start-up. This consists of mostly unbranded food products/ingredients so you will probably not find your favorite branded food. For these cases, you can easily create a custom food (see Custom Foods section).
* Around 400 nutrients are used in the databse, but currently only 5 of them are exposed to the user (energy, protein, carbs, fats, fiber).

## Configuration
The following environment variables should be defined in the scope that the `caloree` command is ran.

```bash
CALOREE_PASSWORD # password used to authenticate with the caloree server. This will probably be the one you configured as `CALOREE_DEFAULT_PASSWORD` when setting up the server.
CALOREE_USERNAME # username used to authenticate with the caloree server. This will probably be the one you configured as `CALOREE_DEFAULT_USERNAME` when setting up the server.
CALOREE_HOST # address of the caloree server
CALOREE_PORT # port of the caloree server
```

## Usage

Help for each command and each flag can be found in the by using the `--help` flag. The rest of this documentation will consist of detailed examples with sample output.

#### Note
On command failure an error will be raised. They dont return appropriate outputs yet.

### Search foods

#### Return the first 10 foods matching the description "raw potato"
```
> caloree food search --page 0 --limit 10 --description "raw potato"
+---+--------+---------------------------------------------------------------------------------------------+
| # | id     | description                                                                                 |
+---+--------+---------------------------------------------------------------------------------------------+
| 0 | 168482 | Sweet potato, raw, unprepared (Includes foods for USDA's Food Distribution Program)         |
| 1 | 169303 | Sweet potato leaves, raw                                                                    |
| 2 | 170026 | Potatoes, flesh and skin, raw                                                               |
| 3 | 170027 | Potatoes, russet, flesh and skin, raw (Includes foods for USDA's Food Distribution Program) |
| 4 | 170028 | Potatoes, white, flesh and skin, raw                                                        |
| 5 | 170029 | Potatoes, red, flesh and skin, raw                                                          |
| 6 | 170032 | Potatoes, raw, skin                                                                         |
+---+--------+---------------------------------------------------------------------------------------------+
```
*Note: page defaults to 0, limit defaults to 25, description defaults to ""*

#### Searching custom foods
```
> caloree custom search
```
*Note: all arguments are the same as normal food searching*

### View Logged Foods

#### View the current day's logged foods
```
> caloree log view 
+---+-------+---------+-------------------------------------------+----------+------------+---------+---------+---------+--------+
| # | time  | id      | description                               | amount   | energy     | protein | carbs   | fat     | fiber  |
+---+-------+---------+-------------------------------------------+----------+------------+---------+---------+---------+--------+
| 0 | 10:45 | c3      | Empty Calories                            | 100.0 gr | 100.0 kcal | 0.0 gr  | 0.0 gr  | 0.0 gr  | 0.0 gr |
| 1 | 10:45 | f173110 | Beef, ground, 93% lean meat / 7% fat, raw | 200.0 gr | 304.0 kcal | 42.0 gr | 0.0 gr  | 14.0 gr | 0.0 gr |
| 2 | 10:45 | f173944 | Bananas, raw                              | 125.0 gr | 111.0 kcal | 1.0 gr  | 29.0 gr | 0.0 gr  | 3.0 gr |
| 3 | 10:45 | f170187 | Nuts, walnuts, english                    | 20.0 gr  | 131.0 kcal | 3.0 gr  | 3.0 gr  | 13.0 gr | 1.0 gr |
+---+-------+---------+-------------------------------------------+----------+------------+---------+---------+---------+--------+

+---+----------+--------------------------+-----------------------------------------------------------------------------+
| # | nutrient | ratio                    | progress                                                                    |
+---+----------+--------------------------+-----------------------------------------------------------------------------+
| 0 | energy   | 646.0 kcal - 2750.0 kcal | ##################--------------------------------------------------------- |
| 1 | protein  | 46.0 gr - 200.0 gr       | ##################--------------------------------------------------------- |
| 2 | carbs    | 31.0 gr - 300.0 gr       | ########------------------------------------------------------------------- |
| 3 | fat      | 27.0 gr - 80.0 gr        | ##########################------------------------------------------------- |
| 4 | fiber    | 5.0 gr - 30.0 gr         | ############--------------------------------------------------------------- |
+---+----------+--------------------------+-----------------------------------------------------------------------------+
```
*Note: 'c' prefix means the following id belongs to a custom food, while 'f' prefix means the following id belongs to a pre-existing food.*

#### Quickly view current days logs
```bash
> caloree # caloree without subcommands and arguments is equivelant to `caloree log view`
```

#### View the logs on the 16th of September 2022, beginning at 22:00 and ending at 23:00, with time rounded to 1 minute
```
> caloree log view --day 2022-09-16 --round 1 --begin 22:00 --end 23:00
+---+-------+---------+------------------------------------------------+----------+------------+---------+---------+---------+--------+
| # | time  | id      | description                                    | amount   | energy     | protein | carbs   | fat     | fiber  |
+---+-------+---------+------------------------------------------------+----------+------------+---------+---------+---------+--------+
| 0 | 22:30 | f171287 | Egg, whole, raw, fresh                         | 200.0 gr | 286.0 kcal | 25.0 gr | 1.0 gr  | 19.0 gr | 0.0 gr |
| 1 | 22:30 | f168463 | Spinach, cooked, boiled, drained, without salt | 300.0 gr | 69.0 kcal  | 9.0 gr  | 11.0 gr | 1.0 gr  | 7.0 gr |
| 2 | 22:30 | f172183 | Egg, white, raw, fresh                         | 70.0 gr  | 36.0 kcal  | 8.0 gr  | 1.0 gr  | 0.0 gr  | 0.0 gr |
| 3 | 22:30 | f168880 | Rice, white, medium-grain, enriched, cooked    | 100.0 gr | 130.0 kcal | 2.0 gr  | 29.0 gr | 0.0 gr  | 0.0 gr |
| 4 | 22:30 | f173944 | Bananas, raw                                   | 60.0 gr  | 53.0 kcal  | 1.0 gr  | 14.0 gr | 0.0 gr  | 2.0 gr |
| 5 | 22:30 | f173420 | Cheese, feta                                   | 55.0 gr  | 146.0 kcal | 8.0 gr  | 2.0 gr  | 12.0 gr | 0.0 gr |
+---+-------+---------+------------------------------------------------+----------+------------+---------+---------+---------+--------+

+---+----------+--------------------------+-----------------------------------------------------------------------------+
| # | nutrient | ratio                    | progress                                                                    |
+---+----------+--------------------------+-----------------------------------------------------------------------------+
| 0 | energy   | 721.0 kcal - 2750.0 kcal | ####################------------------------------------------------------- |
| 1 | protein  | 53.0 gr - 200.0 gr       | ####################------------------------------------------------------- |
| 2 | carbs    | 58.0 gr - 300.0 gr       | ###############------------------------------------------------------------ |
| 3 | fat      | 32.0 gr - 80.0 gr        | ###############################-------------------------------------------- |
| 4 | fiber    | 9.0 gr - 30.0 gr         | #######################---------------------------------------------------- |
+---+----------+--------------------------+-----------------------------------------------------------------------------+
```
*Note: The progress section shows the summed amounts of the logs currently selected. Day defaults to current day, round defaults to 15, begin defaults to 00:00, and end defaults to 24:00*

### Create and alter logs

#### Create a log on the current day at 10:45 consisting of 20 grams of a pre-existing food with id 170187
```
> caloree log add --id f170187 --time 10:45 --amount 20
Ok!
```

#### Delete the second (they are 0-indexed) log of the current day
```
> caloree log delete -n 1
Ok!
```

#### Change the amount of the 15th log on the 16th of September 2022 to 65 grams
```
> caloree log update -d 2022-09-16 -a 65 -n 14
Ok!
```

#### Undo the last log change of the 16th of September 2022 (Addition, Deletion, Modification)
```
> caloree log undo -d 2022-09-16
Ok!
```

### Add and delete custom foods

#### Add a food with its nutrients per 100 grams
```
> caloree custom add --description "Empty Calories" --energy 100 --protein 0 --fat 0 --fiber 0 --carbs 0
Ok!
```

#### Delete the custom food with id 2
```
caloree custom delete --id 2
```
*Note: additions and deletions cannot be undone like logs can*

### Update nutrient targets

#### Update the nutrient targets that are displayed in the progress section
```
> caloree nutrients -e 2750 -p 200 -c 300 --fat 80 --fiber 30
```

