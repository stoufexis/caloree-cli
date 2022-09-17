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
* The foods that are available on startup along with information about their nutrients has been gathered from the [USDA FoodData Central](https://fdc.nal.usda.gov/)

## Caveats
* Secure communication through HTTPS is technically supported, but not implemented yet. Thus, the server should either be installed locally or accessed via other secure communication channels (e.g. ssh tunelling).
* Around 17.000 foods are available on start-up. This consists of mostly unbranded food products/ingredients so you will probably not find your favorite branded food. For these cases, you can easily create a custom food (see Custom Foods section).
* Around 400 nutrients are used in the databse, but currently only 5 of them are exposed to the user (energy, protein, carbs, fats, fiber).

## Usage

### Configuration
The following environment variables should be defined in the scope that the `caloree` command is ran.

```bash
CALOREE_PASSWORD # password used to authenticate with the caloree server. This will probably be the one you configured as `CALOREE_DEFAULT_PASSWORD` when setting up the server.
CALOREE_USERNAME # username used to authenticate with the caloree server. This will probably be the one you configured as `CALOREE_DEFAULT_USERNAME` when setting up the server.
CALOREE_HOST # address of the caloree server
CALOREE_PORT # port of the caloree server
```
