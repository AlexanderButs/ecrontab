-module(crontab_schedule_tests).
-author("alexb").

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

% parse tests
should_throw_if_incorrect_field_value_test() ->
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("/10 * * * *")),
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("-10 * * * *")),
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("10- * * * *")),
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse(",10 * * * *")),
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("/10, * * * *")),
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("* /4 * * *")),
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("* -4 * * *")),
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("* 4- * * *")),
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("* ,4 * * *")),
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("* /4, * * *")),
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("* * /3 * *")),
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("* * -3 * *")),
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("* * 3- * *")),
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("* * ,3 * *")),
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("* * /3, * *")),
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("* * * /3 *")),
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("* * * -3 *")),
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("* * * 3- *")),
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("* * * ,3 *")),
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("* * * /3, *")),
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("* * * * /1")),
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("* * * * -1")),
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("* * * * 1-")),
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("* * * * ,1")),
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("* * * * /1,")).
should_throw_if_lower_value_days_test() ->
  ?assertException(throw, {bad_field, _, lower_then, _}, crontab_schedule:parse("* * 0-5 * *")).
should_throw_if_lower_value_month_test() ->
  ?assertException(throw, {bad_field, _, lower_then, _}, crontab_schedule:parse("* * * 0-5 *")).
should_throw_if_greater_value_minutes_test() ->
  ?assertException(throw, {bad_field, _, higher_then, _}, crontab_schedule:parse("65-71 * * * *")),
  ?assertException(throw, {bad_field, _, higher_then, _}, crontab_schedule:parse("55-71 * * * *")),
  ?assertException(throw, {bad_field, _, higher_then, _}, crontab_schedule:parse("71-55 * * * *")).
should_throw_if_greater_value_hours_test() ->
  ?assertException(throw, {bad_field, _, higher_then, _}, crontab_schedule:parse("* 25-27 * * *")),
  ?assertException(throw, {bad_field, _, higher_then, _}, crontab_schedule:parse("* 19-27 * * *")),
  ?assertException(throw, {bad_field, _, higher_then, _}, crontab_schedule:parse("* 27-19 * * *")).
should_throw_if_greater_value_days_test() ->
  ?assertException(throw, {bad_field, _, higher_then, _}, crontab_schedule:parse("* * 33-35 * *")),
  ?assertException(throw, {bad_field, _, higher_then, _}, crontab_schedule:parse("* * 28-35 * *")),
  ?assertException(throw, {bad_field, _, higher_then, _}, crontab_schedule:parse("* * 35-28 * *")).
should_throw_if_greater_value_month_test() ->
  ?assertException(throw, {bad_field, _, higher_then, _}, crontab_schedule:parse("* * * 19-24 *")),
  ?assertException(throw, {bad_field, _, higher_then, _}, crontab_schedule:parse("* * * 9-14 *")),
  ?assertException(throw, {bad_field, _, higher_then, _}, crontab_schedule:parse("* * * 14-9 *")).
should_throw_if_greater_value_day_of_week_test() ->
  ?assertException(throw, {bad_field, _, higher_then, _}, crontab_schedule:parse("* * * * 15-18")),
  ?assertException(throw, {bad_field, _, higher_then, _}, crontab_schedule:parse("* * * * 5-8")),
  ?assertException(throw, {bad_field, _, higher_then, _}, crontab_schedule:parse("* * * * 8-5")).
should_throw_if_parse_empty_string_test() ->
  ?assertException(throw, empty_string, crontab_schedule:parse("")).
should_throw_if_minute_is_bad_test() ->
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("bad * * * *")).
should_throw_if_hour_is_bad_test() ->
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("* bad * * *")).
should_throw_if_day_is_bad_test() ->
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("* * bad * *")).
should_throw_if_month_is_bad_test() ->
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("* * * bad *")).
should_throw_if_day_of_week_is_bad_test() ->
  ?assertException(throw, {bad_field, _}, crontab_schedule:parse("* * * * bad")).
should_throw_if_fields_are_not_five_test() ->
  ?assertException(throw, invalid_fields_count, crontab_schedule:parse("* * * * * *")).
should_parse_stars_into_tuple_with_all_test() ->
  ?assertEqual({all, all, all, all, all}, crontab_schedule:parse("* * * * *")).
% slash tests
should_parse_slash_with_star_into_tuple_with_segment_minutes_test() ->
  ?assertEqual({[5, 15, 25, 35, 45, 55], all, all, all, all}, crontab_schedule:parse("5/10 * * * *")),
  ?assertEqual({[0, 10, 20, 30, 40, 50], all, all, all, all}, crontab_schedule:parse("*/10 * * * *")),
  ?assertEqual({[0, 15, 30, 45], all, all, all, all}, crontab_schedule:parse("*/15 * * * *")).
should_parse_slash_with_star_into_tuple_with_segment_hours_test() ->
  ?assertEqual({all, [2, 6, 10, 14, 18, 22], all, all, all}, crontab_schedule:parse("* 2/4 * * *")),
  ?assertEqual({all, [0, 4, 8, 12, 16, 20], all, all, all}, crontab_schedule:parse("* */4 * * *")),
  ?assertEqual({all, [0, 6, 12, 18], all, all, all}, crontab_schedule:parse("* */6 * * *")).
should_parse_slash_with_star_into_tuple_with_segment_days_test() ->
  ?assertEqual({all, all, [3, 8, 13, 18, 23, 28], all, all}, crontab_schedule:parse("* * 3/5 * *")),
  ?assertEqual({all, all, [1, 6, 11, 16, 21, 26, 31], all, all}, crontab_schedule:parse("* * */5 * *")),
  ?assertEqual({all, all, [1, 11, 21, 31], all, all}, crontab_schedule:parse("* * */10 * *")).
should_parse_slash_with_star_into_tuple_with_segment_month_test() ->
  ?assertEqual({all, all, all, [2, 5, 8, 11], all}, crontab_schedule:parse("* * * 2/3 *")),
  ?assertEqual({all, all, all, [1, 4, 7, 10], all}, crontab_schedule:parse("* * * */3 *")),
  ?assertEqual({all, all, all, [1, 6, 11], all}, crontab_schedule:parse("* * * */5 *")).
should_parse_slash_with_star_into_tuple_with_segment_day_of_week_test() ->
  ?assertEqual({all, all, all, all, [1, 3, 5]}, crontab_schedule:parse("* * * * 1/2")),
  ?assertEqual({all, all, all, all, [0, 2, 4, 6]}, crontab_schedule:parse("* * * * */2")),
  ?assertEqual({all, all, all, all, [0, 3, 6]}, crontab_schedule:parse("* * * * */3")),
  ?assertEqual({all, all, all, all, [0, 1, 2, 3, 4, 5, 6]}, crontab_schedule:parse("* * * * */1")).
% dash tests
should_parse_dash_into_tuple_with_segment_minutes_test() ->
  ?assertEqual({[10, 11, 12, 13, 14, 15], all, all, all, all}, crontab_schedule:parse("10-15 * * * *")),
  ?assertEqual({[37, 38, 39, 40, 41], all, all, all, all}, crontab_schedule:parse("41-37 * * * *")).
should_parse_dash_into_tuple_with_segment_hours_test() ->
  ?assertEqual({all, [4, 5, 6], all, all, all}, crontab_schedule:parse("* 4-6 * * *")),
  ?assertEqual({all, [7, 8, 9, 10, 11], all, all, all}, crontab_schedule:parse("* 11-7 * * *")).
should_parse_dash_into_tuple_with_segment_days_test() ->
  ?assertEqual({all, all, [5, 6, 7, 8], all, all}, crontab_schedule:parse("* * 5-8 * *")),
  ?assertEqual({all, all, [19, 20, 21, 22, 23], all, all}, crontab_schedule:parse("* * 23-19 * *")).
should_parse_dash_into_tuple_with_segment_month_test() ->
  ?assertEqual({all, all, all, [3, 4, 5, 6, 7], all}, crontab_schedule:parse("* * * 3-7 *")),
  ?assertEqual({all, all, all, [1, 2, 3, 4, 5], all}, crontab_schedule:parse("* * * 5-1 *")).
should_parse_dash_into_tuple_with_segment_day_of_week_test() ->
  ?assertEqual({all, all, all, all, [0, 1, 2]}, crontab_schedule:parse("* * * * 0-2")),
  ?assertEqual({all, all, all, all, [0, 1, 2, 3]}, crontab_schedule:parse("* * * * 3-0")).
% comma tests
should_parse_comma_into_tuple_with_segment_minutes_test() ->
  ?assertEqual({[10, 15, 17, 25], all, all, all, all}, crontab_schedule:parse("10,15,17,25 * * * *")).
should_parse_comma_into_tuple_with_segment_hours_test() ->
  ?assertEqual({all, [1, 5, 17, 23], all, all, all}, crontab_schedule:parse("* 1,5,17,23 * * *")).
should_parse_comma_into_tuple_with_segment_days_test() ->
  ?assertEqual({all, all, [5, 8, 26, 31], all, all}, crontab_schedule:parse("* * 5,8,26,31 * *")).
should_parse_comma_into_tuple_with_segment_month_test() ->
  ?assertEqual({all, all, all, [3, 7, 12], all}, crontab_schedule:parse("* * * 3,7,12 *")).
should_parse_comma_into_tuple_with_segment_day_of_week_test() ->
  ?assertEqual({all, all, all, all, [0, 2, 5]}, crontab_schedule:parse("* * * * 0,2,5")).
% mixed tests
should_parse_mixed_minutes_test() ->
  ?assertEqual({[10, 11, 12, 13, 14, 19, 29, 39, 49, 59], all, all, all, all}, crontab_schedule:parse("10-14,19/10 * * * *")),
  ?assertEqual({[10, 11, 12, 19, 20, 21, 30, 44, 58], all, all, all, all}, crontab_schedule:parse("10-12,19-21,30/14 * * * *")).
should_parse_mixed_hours_test() ->
  ?assertEqual({all, [10, 11, 12, 13, 14, 19, 21, 23], all, all, all}, crontab_schedule:parse("* 10-14,19/2 * * *")),
  ?assertEqual({all, [3, 4, 5, 14, 15, 16, 17, 18, 20, 22], all, all, all}, crontab_schedule:parse("* 3-5,14-17,18/2 * * *")).
should_parse_mixed_days_test() ->
  ?assertEqual({all, all, [5, 6, 7, 19, 23, 27, 31], all, all}, crontab_schedule:parse("* * 5-7,19/4 * *")),
  ?assertEqual({all, all, [3, 4, 5, 14, 15, 16, 17, 19, 22, 25, 28, 31], all, all}, crontab_schedule:parse("* * 3-5,14-17,19/3 * *")).
should_parse_mixed_month_test() ->
  ?assertEqual({all, all, all, [5, 6, 7, 9, 11], all}, crontab_schedule:parse("* * * 5-7,9/2 *")),
  ?assertEqual({all, all, all, [1, 2, 3, 5, 6, 8, 10, 12], all}, crontab_schedule:parse("* * * 1-3,5-6,8/2 *")).
should_parse_mixed_all_test() ->
  ?assertEqual({[10], all, all, [2, 4, 5, 6], all}, crontab_schedule:parse("10 * * February,April-Jun *")),
  ?assertEqual({[45], [16], [1], all, [1]}, crontab_schedule:parse("45 16 1 * Mon")).
% just numer tests
should_parse_just_numbers_test() ->
  ?assertEqual({[10], [20], [30], [6], [4]}, crontab_schedule:parse("10 20 30 6 4")).

% get_next_occurrence tests
should_calculate_next_occurrence_test() ->
  check_occurrence_calculation({{2003,1,1},{0,0,0}}, "* * * * *", {{2003,1,1},{0,1,0}}),
  check_occurrence_calculation({{2003,1,1},{0,1,0}}, "* * * * *", {{2003,1,1},{0,2,0}}),
  check_occurrence_calculation({{2003,1,1},{0,2,0}}, "* * * * *", {{2003,1,1},{0,3,0}}),
  check_occurrence_calculation({{2003,1,1},{0,59,0}}, "* * * * *", {{2003,1,1},{1,0,0}}),
  check_occurrence_calculation({{2003,1,1},{1,59,0}}, "* * * * *", {{2003,1,1},{2,0,0}}),
  check_occurrence_calculation({{2003,1,1},{23,59,0}}, "* * * * *", {{2003,1,2},{0,0,0}}),
  check_occurrence_calculation({{2003,12,31},{23,59,0}}, "* * * * *", {{2004,1,1},{0,0,0}}),

  check_occurrence_calculation({{2003,2,28},{23,59,0}}, "* * * * *", {{2003,3,1},{0,0,0}}),
  check_occurrence_calculation({{2004,2,28},{23,59,0}}, "* * * * *", {{2004,2,29},{0,0,0}}),

  % Minute tests

  check_occurrence_calculation({{2003,1,1},{0,0,0}}, "45 * * * *", {{2003,1,1},{0,45,0}}),

  check_occurrence_calculation({{2003,1,1},{0,0,0}}, "45-47,48,49 * * * *", {{2003,1,1},{0,45,0}}),
  check_occurrence_calculation({{2003,1,1},{0,45,0}}, "45-47,48,49 * * * *", {{2003,1,1},{0,46,0}}),
  check_occurrence_calculation({{2003,1,1},{0,46,0}}, "45-47,48,49 * * * *", {{2003,1,1},{0,47,0}}),
  check_occurrence_calculation({{2003,1,1},{0,47,0}}, "45-47,48,49 * * * *", {{2003,1,1},{0,48,0}}),
  check_occurrence_calculation({{2003,1,1},{0,48,0}}, "45-47,48,49 * * * *", {{2003,1,1},{0,49,0}}),
  check_occurrence_calculation({{2003,1,1},{0,49,0}}, "45-47,48,49 * * * *", {{2003,1,1},{1,45,0}}),

  check_occurrence_calculation({{2003,1,1},{0,0,0}}, "2/5 * * * *", {{2003,1,1},{0,2,0}}),
  check_occurrence_calculation({{2003,1,1},{0,2,0}}, "2/5 * * * *", {{2003,1,1},{0,7,0}}),
  check_occurrence_calculation({{2003,1,1},{0,50,0}}, "2/5 * * * *", {{2003,1,1},{0,52,0}}),
  check_occurrence_calculation({{2003,1,1},{0,52,0}}, "2/5 * * * *", {{2003,1,1},{0,57,0}}),
  check_occurrence_calculation({{2003,1,1},{0,57,0}}, "2/5 * * * *", {{2003,1,1},{1,2,0}}),

  % Hour tests

  check_occurrence_calculation({{2003,12,20},{10,0,0}}, " * 3/4 * * *", {{2003,12,20},{11,0,0}}),
  check_occurrence_calculation({{2003,12,20},{0,30,0}}, " * 3   * * *", {{2003,12,20},{3,0,0}}),
  check_occurrence_calculation({{2003,12,20},{1,45,0}}, "30 3   * * *", {{2003,12,20},{3,30,0}}),

  % Day of month tests

  check_occurrence_calculation({{2003,1,7},{0,0,0}}, "30  *  1 * *", {{2003,2,1},{0,30,0}}),
  check_occurrence_calculation({{2003,2,1},{0,30,0}}, "30  *  1 * *", {{2003,2,1},{1,30,0}}),

  check_occurrence_calculation({{2003,1,1},{0,0,0}}, "10  * 22    * *", {{2003,1,22},{0,10,0}}),
  check_occurrence_calculation({{2003,1,1},{0,0,0}}, "30 23 19    * *", {{2003,1,19},{23,30,0}}),
  check_occurrence_calculation({{2003,1,1},{0,0,0}}, "30 23 21    * *", {{2003,1,21},{23,30,0}}),
  check_occurrence_calculation({{2003,1,1},{0,1,0}}, " *  * 21    * *", {{2003,1,21},{0,0,0}}),
  check_occurrence_calculation({{2003,7,10},{0,0,0}}, " *  * 30,31 * *", {{2003,7,30},{0,0,0}}),

  % Test month rollovers for months with 28,29,30 and 31 days

  check_occurrence_calculation({{2002,2,28},{23,59,0}}, "* * * 3 *", {{2002,3,1},{0,0,0}}),
  check_occurrence_calculation({{2004,2,29},{23,59,0}}, "* * * 3 *", {{2004,3,1},{0,0,0}}),
  check_occurrence_calculation({{2002,3,31},{23,59,0}}, "* * * 4 *", {{2002,4,1},{0,0,0}}),
  check_occurrence_calculation({{2002,4,30},{23,59,0}}, "* * * 5 *", {{2002,5,1},{0,0,0}}),

  % Test month 30,31 days

  check_occurrence_calculation({{2000,1,1},{0,0,0}}, "0 0 15,30,31 * *", {{2000,1,15},{0,0,0}}),
  check_occurrence_calculation({{2000,1,15},{0,0,0}}, "0 0 15,30,31 * *", {{2000,1,30},{0,0,0}}),
  check_occurrence_calculation({{2000,1,30},{0,0,0}}, "0 0 15,30,31 * *", {{2000,1,31},{0,0,0}}),
  check_occurrence_calculation({{2000,1,31},{0,0,0}}, "0 0 15,30,31 * *", {{2000,2,15},{0,0,0}}),

  check_occurrence_calculation({{2000,2,15},{0,0,0}}, "0 0 15,30,31 * *", {{2000,3,15},{0,0,0}}),

  check_occurrence_calculation({{2000,3,15},{0,0,0}}, "0 0 15,30,31 * *", {{2000,3,30},{0,0,0}}),
  check_occurrence_calculation({{2000,3,30},{0,0,0}}, "0 0 15,30,31 * *", {{2000,3,31},{0,0,0}}),
  check_occurrence_calculation({{2000,3,31},{0,0,0}}, "0 0 15,30,31 * *", {{2000,4,15},{0,0,0}}),

  check_occurrence_calculation({{2000,4,15},{0,0,0}}, "0 0 15,30,31 * *", {{2000,4,30},{0,0,0}}),
  check_occurrence_calculation({{2000,4,30},{0,0,0}}, "0 0 15,30,31 * *", {{2000,5,15},{0,0,0}}),

  check_occurrence_calculation({{2000,5,15},{0,0,0}}, "0 0 15,30,31 * *", {{2000,5,30},{0,0,0}}),
  check_occurrence_calculation({{2000,5,30},{0,0,0}}, "0 0 15,30,31 * *", {{2000,5,31},{0,0,0}}),
  check_occurrence_calculation({{2000,5,31},{0,0,0}}, "0 0 15,30,31 * *", {{2000,6,15},{0,0,0}}),

  check_occurrence_calculation({{2000,6,15},{0,0,0}}, "0 0 15,30,31 * *", {{2000,6,30},{0,0,0}}),
  check_occurrence_calculation({{2000,6,30},{0,0,0}}, "0 0 15,30,31 * *", {{2000,7,15},{0,0,0}}),

  check_occurrence_calculation({{2000,7,15},{0,0,0}}, "0 0 15,30,31 * *", {{2000,7,30},{0,0,0}}),
  check_occurrence_calculation({{2000,7,30},{0,0,0}}, "0 0 15,30,31 * *", {{2000,7,31},{0,0,0}}),
  check_occurrence_calculation({{2000,7,31},{0,0,0}}, "0 0 15,30,31 * *", {{2000,8,15},{0,0,0}}),

  check_occurrence_calculation({{2000,8,15},{0,0,0}}, "0 0 15,30,31 * *", {{2000,8,30},{0,0,0}}),
  check_occurrence_calculation({{2000,8,30},{0,0,0}}, "0 0 15,30,31 * *", {{2000,8,31},{0,0,0}}),
  check_occurrence_calculation({{2000,8,31},{0,0,0}}, "0 0 15,30,31 * *", {{2000,9,15},{0,0,0}}),

  check_occurrence_calculation({{2000,9,15},{0,0,0}}, "0 0 15,30,31 * *", {{2000,9,30},{0,0,0}}),
  check_occurrence_calculation({{2000,9,30},{0,0,0}}, "0 0 15,30,31 * *", {{2000,10,15},{0,0,0}}),

  check_occurrence_calculation({{2000,10,15},{0,0,0}}, "0 0 15,30,31 * *", {{2000,10,30},{0,0,0}}),
  check_occurrence_calculation({{2000,10,30},{0,0,0}}, "0 0 15,30,31 * *", {{2000,10,31},{0,0,0}}),
  check_occurrence_calculation({{2000,10,31},{0,0,0}}, "0 0 15,30,31 * *", {{2000,11,15},{0,0,0}}),

  check_occurrence_calculation({{2000,11,15},{0,0,0}}, "0 0 15,30,31 * *", {{2000,11,30},{0,0,0}}),
  check_occurrence_calculation({{2000,11,30},{0,0,0}}, "0 0 15,30,31 * *", {{2000,12,15},{0,0,0}}),

  check_occurrence_calculation({{2000,12,15},{0,0,0}}, "0 0 15,30,31 * *", {{2000,12,30},{0,0,0}}),
  check_occurrence_calculation({{2000,12,30},{0,0,0}}, "0 0 15,30,31 * *", {{2000,12,31},{0,0,0}}),
  check_occurrence_calculation({{2000,12,31},{0,0,0}}, "0 0 15,30,31 * *", {{2001,1,15},{0,0,0}}),

  % Other month tests (including year rollover)

  check_occurrence_calculation({{2003,12,1},{5,0,0}}, "10 * * 6 *", {{2004,6,1},{0,10,0}}),
  check_occurrence_calculation({{2003,1,4},{0,0,0}}, " 1 2 3 * *", {{2003,2,3},{2,1,0}}),
  check_occurrence_calculation({{2002,7,1},{5,0,0}}, "10 * * February,April-Jun *", {{2003,2,1},{0,10,0}}),
  check_occurrence_calculation({{2003,1,1},{0,0,0}}, "0 12 1 6 *", {{2003,6,1},{12,0,0}}),
  check_occurrence_calculation({{1988,9,11},{14,23,0}}, "* 12 1 6 *", {{1989,6,1},{12,0,0}}),
  check_occurrence_calculation({{1988,3,11},{14,23,0}}, "* 12 1 6 *", {{1988,6,1},{12,0,0}}),
  check_occurrence_calculation({{1988,3,11},{14,23,0}}, "* 2,4-8,15 * 6 *", {{1988,6,1},{2,0,0}}),
  check_occurrence_calculation({{1988,3,11},{14,23,0}}, "20 * * january,FeB,Mar,april,May,JuNE,July,Augu,SEPT-October,Nov,DECEM *", {{1988,3,11},{15,20,0}}),

  % Day of week tests

  check_occurrence_calculation({{2003,6,26},{10,0,0}}, "30 6 * * 0", {{2003,6,29},{6,30,0}}),
  check_occurrence_calculation({{2003,6,26},{10,0,0}}, "30 6 * * sunday", {{2003,6,29},{6,30,0}}),
  check_occurrence_calculation({{2003,6,26},{10,0,0}}, "30 6 * * SUNDAY", {{2003,6,29},{6,30,0}}),
  check_occurrence_calculation({{2003,6,19},{0,0,0}}, "1 12 * * 2", {{2003,6,24},{12,1,0}}),
  check_occurrence_calculation({{2003,6,24},{12,1,0}}, "1 12 * * 2", {{2003,7,1},{12,1,0}}),

  check_occurrence_calculation({{2003,6,1},{14,55,0}}, "15 18 * * Mon", {{2003,6,2},{18,15,0}}),
  check_occurrence_calculation({{2003,6,2},{18,15,0}}, "15 18 * * Mon", {{2003,6,9},{18,15,0}}),
  check_occurrence_calculation({{2003,6,9},{18,15,0}}, "15 18 * * Mon", {{2003,6,16},{18,15,0}}),
  check_occurrence_calculation({{2003,6,16},{18,15,0}}, "15 18 * * Mon", {{2003,6,23},{18,15,0}}),
  check_occurrence_calculation({{2003,6,23},{18,15,0}}, "15 18 * * Mon", {{2003,6,30},{18,15,0}}),
  check_occurrence_calculation({{2003,6,30},{18,15,0}}, "15 18 * * Mon", {{2003,7,7},{18,15,0}}),

  check_occurrence_calculation({{2003,1,1},{0,0,0}}, "* * * * Mon",   {{2003,1,6},{0,0,0}}),
  check_occurrence_calculation({{2003,1,1},{12,0,0}}, "45 16 1 * Mon", {{2003,9,1},{16,45,0}}),
  check_occurrence_calculation({{2003,9,1},{23,45,0}}, "45 16 1 * Mon", {{2003,12,1},{16,45,0}}),

  % Leap year tests

  check_occurrence_calculation({{2000,1,1},{12,0,0}}, "1 12 29 2 *", {{2000,2,29},{12,1,0}}),
  check_occurrence_calculation({{2000,2,29},{12,1,0}}, "1 12 29 2 *", {{2004,2,29},{12,1,0}}),
  check_occurrence_calculation({{2004,2,29},{12,1,0}}, "1 12 29 2 *", {{2008,2,29},{12,1,0}}),

  % Non-leap year tests

  check_occurrence_calculation({{2000,1,1},{12,0,0}}, "1 12 28 2 *", {{2000,2,28},{12,1,0}}),
  check_occurrence_calculation({{2000,2,28},{12,1,0}}, "1 12 28 2 *", {{2001,2,28},{12,1,0}}),
  check_occurrence_calculation({{2001,2,28},{12,1,0}}, "1 12 28 2 *", {{2002,2,28},{12,1,0}}),
  check_occurrence_calculation({{2002,2,28},{12,1,0}}, "1 12 28 2 *", {{2003,2,28},{12,1,0}}),
  check_occurrence_calculation({{2003,2,28},{12,1,0}}, "1 12 28 2 *", {{2004,2,28},{12,1,0}}),
  check_occurrence_calculation({{2004,2,28},{12,1,0}}, "1 12 28 2 *", {{2005,2,28},{12,1,0}}).

check_occurrence_calculation(StartTime, CronExpression, NextTime) ->
  Cron = crontab_schedule:parse(CronExpression),
  ?assertEqual(NextTime, crontab_schedule:get_next_occurrence(Cron, StartTime)).

% get_next_occurrence_after_ms tests
should_calculate_next_occurrence_in_milliseconds_test() ->
  ?assertEqual(60000, crontab_schedule:get_next_occurrence_after_ms({all, all, all, all, all}, {{2000,1,1},{0,0,0}})),
  ?assertEqual(60 * 60000, crontab_schedule:get_next_occurrence_after_ms({all, [1], all, all, all}, {{2000,1,1},{0,0,0}})),
  ?assertEqual(24 * 60 * 60000, crontab_schedule:get_next_occurrence_after_ms({all, all, [2], all, all}, {{2000,1,1},{0,0,0}})).

should_calculate_next_occurrances_test() ->
  check_occurrences_calculation({{2004,1,1},{0,0,0}}, {{2004,1,1},{0,5,0}},
                                "* * * * *",
                                [{{2004,1,1},{0,1,0}}, {{2004,1,1},{0,2,0}}, {{2004,1,1},{0,3,0}}, {{2004,1,1},{0,4,0}}, {{2004,1,1},{0,5,0}}]).

check_occurrences_calculation(StartTime, EndTime, CronExpression, NextTimeList) ->
  Cron = crontab_schedule:parse(CronExpression),
  ?assertEqual(NextTimeList, crontab_schedule:get_next_occurrences(Cron, StartTime, EndTime)).
