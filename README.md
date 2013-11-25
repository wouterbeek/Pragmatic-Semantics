# Pragmatic Semantics for the Web of Data (PraSem)

This will combine the projects that are conducted with the context of
the NWO-sunded project PraSem.

# Git

Since this project uses many submodules,
and some people are still unfamiliar with them,
we enumerate the default way of pulling changes from the PraSem repository.

Cloning the repository (this is done only once):
~~~
git clone https://github.com/wouterbeek/PraSem.git
git submodule init
git submodule update
~~~

Updating the repository (this is done regularly in order to stay up to date):
~~~
git pull --recurse-submodules
git submodule update --remote
~~~

