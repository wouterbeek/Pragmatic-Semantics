:- module(web_threads, []).

/** <module> Web threads

Web-based overview of the currently running threads.

@author Wouter Beek
@version 2014/02
*/

:- use_remote_module(html(html_table)).
:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_remote_module(server(web_modules)).

:- http_handler(root(threads), web_threads, []).

user:web_module('Threads', web_threads).



web_threads(_Request):-
  reply_html_page(
    app_style,
    title('Thread overview'),
    html(\thread_overview)
  ).


thread_overview -->
  {
    aggregate_all(
      set(Alias),
      (
        thread_property(Id, status(running)),
        thread_property(Id, alias(Alias))
      ),
      Aliases
    ),
    findall(
      [Key|Values],
      (
        statistic(Key, _),
        findall(
          Value,
          (
            member(Alias, Aliases),
            thread_statistics(Alias, Key, Value)
          ),
          Values
        )
      ),
      Rows
    )
  },
  html_table(
    [header_column(true),header_row(true)],
    html('Statistics for all threads.'),
    [['Statistic'|Aliases]|Rows]
  ).


statistic(atoms,           'Total number of defined atoms').
statistic(agc,             'Number of atom garbage collections performed').
statistic(agc_gained,      'Number of atoms removed').
statistic(agc_time,        'Time spent in atom garbage collections').
statistic(c_stack,         'System (C-) stack limit. 0 if not known.').
statistic(clauses,         'Total number of clauses in the program').
statistic(codes,           'Total size of (virtual) executable code in words').
statistic(cputime,         '(User) CPU time since thread was started in seconds').
statistic(functors,        'Total number of defined name/arity pairs').
statistic(global,          'Allocated size of the global stack in bytes').
statistic(globalused,      'Number of bytes in use on the global stack').
statistic(globallimit,     'Size to which the global stack is allowed to grow').
statistic(global_shifts,   'Number of global stack expansions').
statistic(heapused,        'Bytes of heap in use by Prolog (0 if not maintained)').
%statistic(heap_gc,         'Number of heap garbage collections performed. Only provided if SWI-Prolog is configured with Boehm-GC. See also garbage_collect_heap/0.').
statistic(inferences,      'Total number of passes via the call and redo ports since Prolog was started').
statistic(local,           'Allocated size of the local stack in bytes').
statistic(local_shifts,    'Number of local stack expansions').
statistic(localused,       'Number of bytes in use on the local stack').
statistic(locallimit,      'Size to which the local stack is allowed to grow').
statistic(modules,         'Total number of defined modules').
statistic(process_cputime, '(User) CPU time since Prolog was started in seconds').
statistic(trail,           'Allocated size of the trail stack in bytes').
statistic(trail_shifts,    'Number of trail stack expansions').
statistic(traillimit,      'Size to which the trail stack is allowed to grow').
statistic(trailused,       'Number of bytes in use on the trail stack').
statistic(shift_time,      'Time spent in stack-shifts').
statistic(stack,           'Total memory in use for stacks in all threads').
statistic(thread_cputime,  'MT-version: seconds CPU time used by finished threads. Supported on Windows-NT and later, Linux and possibly a few more. Verify it gives plausible results before using.').
statistic(threads,         'MT-version: number of active threads').
statistic(threads_created, 'MT-version: number of created threads').

