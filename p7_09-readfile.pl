% readfile.pl   werner.hett@hta-bi.bfh.ch  
% Time-stamp: <8-Oct-2000 15:25 hew>

% Auxiliary predicate for reading a text file and splitting the text 
% into lines. Cope with the different end-of-line conventions.
% Should work with UNIX, DOS/Windows, and Mac file system.


% read_lines(File,Lines) :- read the text file File and split the text
% into lines. Lines is a list of char-lists, each of them being a text line.
% (+,-) (atom, list-of-charlists)  

read_lines(File,Lines) :-
   seeing(Old), see(File), 
   get_char(X), read_file(X,CharList),  % read the whole file into a charlist
   parse_charlist(CharList-[],Lines),   % parse lines using difference lists
   see(Old).

read_file(end_of_file,[]) :- !.
read_file(X,[X|Xs]) :- get_char(Y), read_file(Y,Xs).

parse_charlist(T-T,[]) :- !.
parse_charlist(X1-X4,[L|Ls]) :- 
   parse_line(X1-X2,L), 
   parse_eol(X2-X3), !,
   parse_charlist(X3-X4,Ls).

parse_eol([]-[]) :- !.           % no end-of-line at end-of-file
parse_eol(['\r','\n'|R]-R) :- !. % DOS/Windows
parse_eol(['\n','\r'|R]-R) :- !. % Mac (?)
parse_eol(['\r'|R]-R) :- !.      % Mac (?)
parse_eol(['\n'|R]-R).           % UNIX

parse_line([]-[],[]) :- !.       % no end-of-line at end-of-file
parse_line([X|X1]-[X|X1],[]) :- eol_char(X), !.
parse_line([X|X1]-X2,[X|Xs]) :- \+ eol_char(X), parse_line(X1-X2,Xs).

eol_char('\r').
eol_char('\n').
