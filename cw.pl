%%% FINAL ASSIGNMENT COMS30106 %%%

/*
 * The following is a (semi-realistic) train timetable, represented in Prolog. 
 * The meaning of e.g. the first Prolog fact is that 54 minutes past each hour 
 * (say N:54) a train leaves Amsterdam, arriving 2 hours and 38 minutes later
 * (N+3:32) in Brussels, and arriving 4 hours and 11 minutes after its Amsterdam 
 * departure (N+5:05) in Paris. Assume that each of these trains runs every hour. 
 * Also assume that changing trains requires at least 10 minutes, 
 * and that a train leaves a station the same time it arrives. 
 */

:-op(600,xfy,':').
:-op(700,xfy,'/').

% Amsterdam <-> Paris
line([amsterdam/0:54,brussels/3:32,paris/5:05]).
line([paris/0:55,brussels/2:20,amsterdam/5:08]).

% Eurostar
line([london/0:53,paris/4:53]).
line([london/0:27,brussels/4:10]).
line([paris/0:19,london/2:13]).
line([brussels/0:56,london/2:39]).

% Bristol <-> London
line([bristol/0:15,bath/0:26,reading/1:27,london/2:00]).
line([london/0:30,reading/0:54,bath/1:56,bristol/2:10]).

% London <-> York
line([london/0:00,doncaster/1:40,york/2:04]).
line([york/0:02,doncaster/0:34,london/2:18]).

% Bristol <-> York
line([bristol/0:14,birmingham/1:54,doncaster/3:45,york/4:10]).
line([york/0:06,doncaster/0:31,birmingham/2:18,bristol/3:58]).

% Bristol <-> Manchester
line([bristol/0:10,birmingham/1:45,manchester/3:39]).
line([manchester/0:16,birmingham/2:03,bristol/3:30]).

% Manchester <-> York
line([manchester/0:50,leeds/1:51,york/2:22]).
line([york/0:37,leeds/1:05,manchester/2:06]).
