:- use_module(library(prolog_pack)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(socket)).

% Pretends to be a pack server and a git server.

:- http_handler('/pack/query', pack_query, []).

:- http_handler('/packs', pack_files, [prefix]).

:- http_handler('/from_git.git', serve_git, [prefix]).

% Also pretend to be GitHub.

:- http_handler('/user/from_github/archive/v0.0.1.zip', serve_github, [prefix]).

socket:tcp_connect_hook(Socket, 'github.com':80, Streams):-
	tcp_connect(Socket, 'localhost':3000),
	tcp_open_socket(Socket, Streams).

pack_query(Request):-
	memberchk(input(Stream), Request),
	read_term(Stream, Query, []),
	respond_query(Query).

respond_query(locate(standalone)):- !,
	respond_term(true(['0.0.1'-['http://localhost:3000/packs/standalone-0.0.1.tgz']])).

respond_query(locate(tampered)):- !,
	respond_term(true(['0.0.1'-['http://localhost:3000/packs/tampered-0.0.1.tgz']])).

respond_query(locate(dependant)):- !,
	respond_term(true(['0.0.1'-['http://localhost:3000/packs/dependant-0.0.1.tgz']])).

respond_query(locate(foreign)):- !,
	respond_term(true(['0.0.1'-['http://localhost:3000/packs/foreign-0.0.1.tgz']])).

% Produces invalid response term.

respond_query(locate(mumble)):- !,
	format('Content-Type: text/plain~n~n'),
	format('Mumble mumble').

% Unknown packs get response "false".

respond_query(locate(_)):-
	respond_term(false).

% Reports wrong hash for the "tampered" package.

respond_query(install(Url, Hash, _)):-
	Url = 'http://localhost:3000/packs/tampered-0.0.1.tgz', !,
	respond_term(exception(pack(modified_hash(
		Hash-Url,'2807f87abcefb2ae75275d30e12766ce80a48827'-[Url])))).

% Dependant has dependencies.

respond_query(install(Url, _, _)):-
	Url = 'http://localhost:3000/packs/dependant-0.0.1.tgz', !,
	respond_term(true([downloads(9001),
		dependency(standalone, standalone, '0.0.1', [
			'http://localhost:3000/packs/standalone-0.0.1.tgz'],[downloads(9001)])])).

respond_query(install(_, _, _)):-
	respond_term(true([downloads(9001)])).

respond_term(Term):-
	format('Content-Type: text/x-prolog~n~n'),
	write_term(Term, [fullstop(true), quoted(true)]).

% Sends actual pack file.

pack_files(Request):-
	memberchk(path(Path), Request),
	file_base_name(Path, Name),
	atomic_list_concat([testpack, Name], '/', Local),
	http_reply_file(Local, [], Request).

% Serves git repository.

serve_git(Request):-
	memberchk(path(Path), Request),
	atom_concat('/from_git.git/', GitPath, Path),
	atomic_list_concat([testpack, 'from-git',
		'bare.git', GitPath], '/', Local),
	http_reply_file(Local, [], Request).

% Serves file for install-from-GH.

serve_github(Request):-
	http_reply_file('testpack/from-github/v0.0.1.zip', [], Request).

:- dynamic
	started/0.

:- (\+ started
	->  http_server(http_dispatch, [port(3000)]),
		asserta(started)
	; true).

% Make pack_install to use the local server.

:- set_setting(prolog_pack:server, 'http://localhost:3000/pack/').

%% before_call(+InputAtom, -Handle, -Input, -Output, -OldInput, -OldOutput) is det.
%
% Sets up environment to call an interactive predicate.
% Replaces user_input and user_output streams.

before_call(Atom, Handle, Input, Output, OldInput, OldOutput):-
	new_memory_file(Handle),
	open_memory_file(Handle, write, Output),
	readable_atom(Atom, Input),
	current_input(OldInput),
	current_output(OldOutput),
	set_stream(Output, alias(user_output)),
	set_stream(Input, alias(user_input)).

%% after_call(+Handle, +Input, +Output, +OldInput, +OldOutput, -OutputAtom) is det.
%
% Restores the environment. Retrieves the predicate output.
% Restores user_input and user_output streams.

after_call(Handle, Input, Output, OldInput, OldOutput, Atom):-
	close(Input),
	close(Output),
	set_stream(OldOutput, alias(user_output)),
	set_stream(OldInput, alias(user_input)),
	memory_file_to_atom(Handle, Atom),
	free_memory_file(Handle).

:- meta_predicate(call_interactive(+, 0, -)).

%% call_interactive(+InputAtom, +Goal, -Result) is det.
%
% Calls the interactive predicate with user input from InputAtom
% and captured output in Result. Result is one of:
% ok(Output), ex(Exception, Output), fail(Output).

call_interactive(InputAtom, Goal, Result):-
	setup_call_cleanup(
		before_call(InputAtom, Handle, Input, Output, OldInput, OldOutput),
		(   catch(call_with_time_limit(10, Goal), E, true)
			->  (   var(E)
				->  Result = ok(OutputAtom)
				;   Result = ex(E, OutputAtom))
			;   Result = fail(OutputAtom)
		),
		after_call(Handle, Input, Output, OldInput, OldOutput, OutputAtom)).

% Turns atom into a stream.

readable_atom(Atom, Stream):-
	atom_to_memory_file(Atom, Handle),
	open_memory_file(Handle, read, Stream, [free_on_close(true)]).

% Drops temporary directory used
% for installing packs.

ensure_clean:-
	pack_test_location(Location),
	retractall('$pack':pack(_, _)), % internal pack registry
	retractall('$pack':pack_dir(_, _, _)), % internal pack registry
	(   exists_directory(Location)
	->  delete_directory_and_contents(Location)
	;   true),
	make_directory(Location).

% Location where packs are installed to
% during test runs.

pack_test_location('/tmp/pack-test-location').

% Validates result from interactive
% predicate call.

validate_result(Result, ExpectedLines):-
	(   Result = ok(Output)
	->  validate_output(Output, ExpectedLines)
	;   throw(error(erroneus_result(Result)))).

validate_output(Text, ExpectedLines):-
	normalize_output(Text, Lines),
	(   Lines = ExpectedLines
	->  true
	;   atomic_list_concat(Lines, '\n', Actual),
		format(user_error, 'Actual output: ~n~n~w~n~n', [Actual]),
		atomic_list_concat(ExpectedLines, '\n', Expected),
		format(user_error, 'Expected output: ~n~n~w~n~n', [Expected]),
		throw(error(invalid_command_output))).

% Normalizes given text into lines.

normalize_output(Text, Normalized):-
	split_string(Text, "\n", "\r \t", Lines),
	exclude(empty, Lines, Normalized).

empty("").

% Checks that the pack is installed
% into the given location.
% Checks that:
%   pack code directory (/prolog) exists
%   '$pack':pack(Name, PackDir) is set
%   user:file_search_path(library, PackCodeDir) succeeds

validate_installed(Location, Name):-
	atomic_list_concat([Location, Name], '/', PackDir),
	atomic_list_concat([PackDir, prolog], '/', PackCodeDir),
	(   exists_directory(PackCodeDir)
	->  true
	;   throw(error(pack_code_dir_not_exists(PackCodeDir)))),
	(   '$pack':pack(Name, PackDir)
	->  true
	;   throw(error(pack_in_registry_not_set(Name)))),
	(   user:file_search_path(library, PackCodeDir)
	->  true
	;   throw(error(pack_in_file_search_path_not_set(Name)))).

% Checks that the pack is not installed.

validate_not_installed(Location, Name):-
	atomic_list_concat([Location, Name], '/', PackDir),
	atomic_list_concat([PackDir, prolog], '/', PackCodeDir),
	(   exists_directory(PackDir)
	->  throw(error(pack_dir_exists(PackDir)))
	;   true),
	(   '$pack':pack(Name, _)
	->  throw(error(pack_in_registry_set(Name)))
	;   true),
	(   user:file_search_path(library, PackCodeDir)
	->  throw(error(pack_in_file_search_path_set(Name)))
	;   true).

:- begin_tests(pack_install).

install_output([
	"Install standalone@0.0.1 from http://localhost:3000/packs/standalone-0.0.1.tgz Y/n?",
	"Install \"standalone-0.0.1.tgz\" (408 bytes) Y/n?"
]).

test('Install to specific location', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install(standalone, [package_directory(Location)]),
	call_interactive('yy', Goal, Result),
	install_output(Output),
	validate_result(Result, Output),
	validate_installed(Location, standalone).

test('Install to specific location, non-interactive', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install(standalone, [package_directory(Location), interactive(false)]),
	call_interactive('', Goal, Result),
	validate_result(Result, []),
	validate_installed(Location, standalone).

test('Install to specific location, have correct pack_property', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install(standalone, [package_directory(Location)]),
	call_interactive('yy', Goal, InstallResult),
	install_output(Output),
	validate_result(InstallResult, Output),
	validate_installed(Location, standalone),
	assertion(pack_property(standalone, directory('/tmp/pack-test-location/standalone'))),
	assertion(pack_property(standalone, title('Pack with no dependencies'))),
	assertion(pack_property(standalone, version('0.0.1'))),
	assertion(pack_property(standalone, author('Test pack author', 'pack@example.com'))),
	assertion(pack_property(standalone, download('http://localhost:3000/packs/standalone-0.0.1.tgz'))).

% With unknown pack the call just fails.

test('Install unknown pack', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install(unknown, [package_directory(Location)]),
	call_interactive('', Goal, Result),
	assertion(Result = fail('')),
	validate_not_installed(Location, unknown).

tampered_output([
	"Install tampered@0.0.1 from http://localhost:3000/packs/tampered-0.0.1.tgz Y/n?",
	"Pack may be compromised.  Continue anyway y/N?",
	"Install \"tampered-0.0.1.tgz\" (393 bytes) Y/n?"
]).

test('Install tampered pack', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install(tampered, [package_directory(Location)]),
	call_interactive('yyy', Goal, Result),
	tampered_output(Output),
	validate_result(Result, Output),
	validate_installed(Location, tampered).

test('Install tampered pack, discard', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install(tampered, [package_directory(Location)]),
	call_interactive('yN', Goal, Result),
	assertion(Result = fail(_)),
	Result = fail(Output),
	validate_output(Output, [
		"Install tampered@0.0.1 from http://localhost:3000/packs/tampered-0.0.1.tgz Y/n?",
		"Pack may be compromised.  Continue anyway y/N?"
	]).
	%validate_not_installed(Location, tampered). % FIXME leaves empty directory behind.

test('Install tampered pack, non-interactive', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install(tampered, [package_directory(Location), interactive(false)]),
	call_interactive('', Goal, Result),
	assertion(Result = fail('')).
	%validate_not_installed(Location, tampered). % FIXME leaves empty directory behind.

test('Install pack, invalid server response', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install(mumble, [package_directory(Location)]),
	call_interactive('', Goal, Result),
	assertion(Result = fail('')),
	validate_not_installed(Location, mumble).

test('Install local pack', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install('testpack/standalone-0.0.1.tgz', [package_directory(Location)]),
	call_interactive('', Goal, Result),
	validate_result(Result, []),
	validate_installed(Location, standalone).

test('Install local directory', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install('testpack/standalone', [package_directory(Location)]),
	call_interactive('', Goal, Result),
	validate_result(Result, []),
	validate_installed(Location, standalone).

% TODO test from local directory containing a version number.
% TODO test with invalid pack.pl.
% TODO test with 'n' as confirmation answer.

test('Install from URL, do not verify', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install('http://localhost:3000/packs/standalone-0.0.1.tgz',
		[package_directory(Location)]),
	call_interactive('ny', Goal, Result),
	validate_result(Result, [
		"Verify package status (anonymously)",
		"at \"http://localhost:3000/pack/query\" Y/n?",
		"Install \"standalone-0.0.1.tgz\" (408 bytes) Y/n?"
	]),
	validate_installed(Location, standalone).

git_install_output([
	"Verify package status (anonymously)",
	"at \"http://localhost:3000/pack/query\" Y/n?",
	"Activate pack \"from_git\" Y/n?"
]).

test('Install from Git URL, do not verify', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install('http://localhost:3000/from_git.git',
		[package_directory(Location)]),
	call_interactive('ny', Goal, Result),
	git_install_output(Output),
	validate_result(Result, Output),
	validate_installed(Location, from_git).

test('Upgrade Git package', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install('http://localhost:3000/from_git.git',
		[package_directory(Location)]),
	call_interactive('ny', Goal, Result),
	git_install_output(Output),
	validate_result(Result, Output),
	validate_installed(Location, from_git),
	call_interactive('', pack_upgrade(from_git), ResultUpgrade),
	assertion(ResultUpgrade = ok('')),
	validate_installed(Location, from_git).

test('Install from GitHub URL', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install('http://github.com/user/from_github/archive/v0.0.1.zip',
		[package_directory(Location)]),
	call_interactive('yy', Goal, Result),
	validate_result(Result, [
		"Verify package status (anonymously)",
		"at \"http://localhost:3000/pack/query\" Y/n?",
		"Install \"from_github-0.0.1.zip\" (715 bytes) Y/n?"
	]),
	validate_installed(Location, from_github).

foreign_install_output([
	"Install foreign@0.0.1 from http://localhost:3000/packs/foreign-0.0.1.tgz Y/n?",
	"Install \"foreign-0.0.1.tgz\" (602 bytes) Y/n?"
]).

% Could check for more parameter passign.

test('Install foreign pack', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install(foreign, [package_directory(Location)]),
	call_interactive('yy', Goal, Result),
	foreign_install_output(Output),
	validate_result(Result, Output),
	validate_installed(Location, foreign),
	atomic_list_concat([Location, foreign, 'version.pl'], '/', VersionFile),
	assertion(exists_file(VersionFile)),
	read_file_to_string(VersionFile, Version, []),
	current_prolog_flag(version, Actual),
	atom_number(Version, VersionNumber),
	assertion(VersionNumber = Actual).

test('Rebuild foreign pack', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install(foreign, [package_directory(Location)]),
	call_interactive('yy', Goal, ResultInstall),
	foreign_install_output(Output),
	validate_result(ResultInstall, Output),
	validate_installed(Location, foreign),
	call_interactive('', pack_rebuild(foreign), ResultRebuild),
	validate_result(ResultRebuild, []),
	atomic_list_concat([Location, foreign, 'version.pl'], '/', VersionFile),
	assertion(exists_file(VersionFile)),
	read_file_to_string(VersionFile, Version, []),
	current_prolog_flag(version, Actual),
	atom_number(Version, VersionNumber),
	assertion(VersionNumber = Actual).

test('Remove pack', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install(standalone, [package_directory(Location)]),
	call_interactive('yy', Goal, ResultInstall),
	install_output(Output),
	validate_result(ResultInstall, Output),
	validate_installed(Location, standalone),
	call_interactive('', pack_remove(standalone), ResultRemove),
	validate_result(ResultRemove, []),
	validate_not_installed(Location, standalone).

% Throws error.

test('Remove non-existing pack', [setup(ensure_clean)]):-
	call_interactive('', pack_remove(standalone), ResultRemove),
	assertion(ResultRemove = ex(error(existence_error(pack, standalone), _), '')).

test('Upgrade in specific location, no upgrade(true) set', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install(standalone, [package_directory(Location)]),
	call_interactive('yy', Goal, ResultInstall),
	install_output(OutputInstall),
	validate_result(ResultInstall, OutputInstall),
	validate_installed(Location, standalone),
	call_interactive('y', Goal, ResultUpgrade),
	assertion(ResultUpgrade = fail(OutputUpgrade)),
	ResultUpgrade = fail(OutputUpgrade),
	validate_output(OutputUpgrade, [
		"Install standalone@0.0.1 from http://localhost:3000/packs/standalone-0.0.1.tgz Y/n?"
	]),
	validate_installed(Location, standalone).

% Asks same things as normal install.

test('Upgrade in specific location, upgrade(true) set', [setup(ensure_clean)]):-
	pack_test_location(Location),
	InstallGoal = pack_install(standalone, [package_directory(Location)]),
	call_interactive('yy', InstallGoal, ResultInstall),
	install_output(OutputInstall),
	validate_result(ResultInstall, OutputInstall),
	validate_installed(Location, standalone),
	UpgradeGoal = pack_install(standalone, [package_directory(Location), upgrade(true)]),
	call_interactive('yy', UpgradeGoal, ResultUpgrade),
	validate_result(ResultUpgrade, OutputInstall),
	validate_installed(Location, standalone).

test('Upgrade in specific location, interactive(false) set', [setup(ensure_clean)]):-
	pack_test_location(Location),
	InstallGoal = pack_install(standalone, [package_directory(Location)]),
	call_interactive('yy', InstallGoal, ResultInstall),
	install_output(OutputInstall),
	validate_result(ResultInstall, OutputInstall),
	validate_installed(Location, standalone),
	UpgradeGoal = pack_install(standalone, [package_directory(Location), interactive(false)]),
	call_interactive('', UpgradeGoal, ResultUpgrade),
	assertion(ResultUpgrade = fail('')),
	validate_installed(Location, standalone).

test('Upgrade in specific location, interactive(false) set, upgrade(true) set', [setup(ensure_clean)]):-
	pack_test_location(Location),
	InstallGoal = pack_install(standalone, [package_directory(Location)]),
	call_interactive('yy', InstallGoal, ResultInstall),
	install_output(OutputInstall),
	validate_result(ResultInstall, OutputInstall),
	validate_installed(Location, standalone),
	UpgradeGoal = pack_install(standalone,
		[package_directory(Location), interactive(false), upgrade(true)]),
	call_interactive('', UpgradeGoal, ResultUpgrade),
	validate_result(ResultUpgrade, []),
	validate_installed(Location, standalone).

dep_output([
	"Install dependant@0.0.1 from http://localhost:3000/packs/dependant-0.0.1.tgz Y/n?",
	"What do you wish to do",
	"(1) * Install proposed dependencies",
	"(2)   Only install requested package",
	"(3)   Cancel",
	"Your choice?",
	"Install \"dependant-0.0.1.tgz\" (420 bytes) Y/n?"
]).

test('Install with dependencies', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install(dependant, [package_directory(Location)]),
	call_interactive('y1y', Goal, Result),
	dep_output(Expected),
	validate_result(Result, Expected),
	validate_installed(Location, dependant),
	validate_installed(Location, standalone).

test('Install with dependencies, no dependencies', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install(dependant, [package_directory(Location)]),
	call_interactive('y2y', Goal, Result),
	dep_output(Expected),
	validate_result(Result, Expected),
	validate_installed(Location, dependant),
	validate_not_installed(Location, standalone).

test('Install with dependencies, non-interactive', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install(dependant, [package_directory(Location), interactive(false)]),
	call_interactive('', Goal, Result),
	validate_result(Result, []),
	validate_installed(Location, dependant),
	validate_installed(Location, standalone).

test('Remove pack but dependencies stay', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install(dependant, [package_directory(Location)]),
	call_interactive('y1y', Goal, ResultInstall),
	dep_output(Expected),
	validate_result(ResultInstall, Expected),
	validate_installed(Location, dependant),
	validate_installed(Location, standalone),
	call_interactive('', pack_remove(dependant), ResultRemove),
	validate_result(ResultRemove, []),
	validate_not_installed(Location, dependant),
	validate_installed(Location, standalone).

dep_remove_output([
	"Please select an action:",
	"(1)   Only remove package standalone (break dependencies)",
	"(2)   Remove package standalone and 1 dependencies",
	"(3) * Cancel",
	"Your choice?"
]).

test('Remove pack dependency, removes dependants', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install(dependant, [package_directory(Location)]),
	call_interactive('y1y', Goal, ResultInstall),
	dep_output(Expected),
	validate_result(ResultInstall, Expected),
	validate_installed(Location, dependant),
	validate_installed(Location, standalone),
	call_interactive('2', pack_remove(standalone), ResultRemove),
	dep_remove_output(ExpectedRemove),
	validate_result(ResultRemove, ExpectedRemove),
	validate_not_installed(Location, standalone),
	validate_not_installed(Location, dependant).

test('Remove pack dependency, breaks dependants', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install(dependant, [package_directory(Location)]),
	call_interactive('y1y', Goal, ResultInstall),
	dep_output(Expected),
	validate_result(ResultInstall, Expected),
	validate_installed(Location, dependant),
	validate_installed(Location, standalone),
	call_interactive('1', pack_remove(standalone), ResultRemove),
	dep_remove_output(ExpectedRemove),
	validate_result(ResultRemove, ExpectedRemove),
	validate_installed(Location, dependant),
	validate_not_installed(Location, standalone).

% Wont ask about already install dependency.

test('Install pack, dependencies already installed', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install(standalone, [package_directory(Location)]),
	call_interactive('yy', Goal, Result),
	install_output(Output),
	validate_result(Result, Output),
	DependantGoal = pack_install(dependant, [package_directory(Location)]),
	call_interactive('yy', DependantGoal, DependantResult),
	validate_result(DependantResult, [
		"Install dependant@0.0.1 from http://localhost:3000/packs/dependant-0.0.1.tgz Y/n?",
		"Install \"dependant-0.0.1.tgz\" (420 bytes) Y/n?"
	]),
	validate_installed(Location, dependant).

% Dependencies do not get installed.

test('Install from URL, have dependencies, do not verify', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install('http://localhost:3000/packs/dependant-0.0.1.tgz',
		[package_directory(Location)]),
	call_interactive('ny', Goal, Result),
	validate_result(Result, [
		"Verify package status (anonymously)",
		"at \"http://localhost:3000/pack/query\" Y/n?",
		"Install \"dependant-0.0.1.tgz\" (420 bytes) Y/n?"
	]),
	validate_installed(Location, dependant),
	validate_not_installed(Location, standalone).

% Dependencies do get installed.

test('Install from URL, have dependencies, do not verify', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install('http://localhost:3000/packs/dependant-0.0.1.tgz',
		[package_directory(Location)]),
	call_interactive('y1y', Goal, Result),
	validate_result(Result, [
		"Verify package status (anonymously)",
		"at \"http://localhost:3000/pack/query\" Y/n?",
		"What do you wish to do",
		"(1) * Install proposed dependencies",
		"(2)   Only install requested package",
		"(3)   Cancel",
		"Your choice?",
		"Install \"dependant-0.0.1.tgz\" (420 bytes) Y/n?"
	]),
	validate_installed(Location, dependant),
	validate_installed(Location, standalone).

% Dependencies do get installed.

test('Install from URL, have dependencies, non-interactive', [setup(ensure_clean)]):-
	pack_test_location(Location),
	Goal = pack_install('http://localhost:3000/packs/dependant-0.0.1.tgz',
		[package_directory(Location), interactive(false)]),
	call_interactive('', Goal, Result),
	validate_result(Result, []),
	validate_installed(Location, dependant),
	validate_installed(Location, standalone).

:- end_tests(pack_install).
