# Pragmatic Semantics for the Web of Data (PraSem)

This will combine the projects that are conducted with the context of
the NWO-sunded project PraSem.

# Git

Since this project uses many submodules,
and some people are still unfamiliar with them,
we enumerate the default way of pulling changes from the PraSem repository.

Cloning the repository (this is done only once):
~~~{.sh}
git clone https://github.com/wouterbeek/PraSem.git
git submodule init
git submodule update
~~~

Updating the repository (this is done regularly in order to stay up to date):
~~~{.sh}
git pull --recurse-submodules
git submodule update --remote
~~~

# Startup

Executing [1] shows a list of available projects to load.
Then execute [2] with your project of choice filled in for `PROJECT_NAME`.

~~~{.sh}
[1]   swipl -s debug.pl
[2]   swipl -s debug.pl PROJECT_NAME
~~~

