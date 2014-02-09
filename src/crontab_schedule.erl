-module(crontab_schedule).
-author("alexb").

%% API
-export([
  parse/1,
  get_next_occurrence/2,
  get_next_occurrence_after_ms/2,
  get_next_occurrences/3
]).

-define(DAY_IN_SECONDS, 86400).
-define(HOUR_IN_SECONDS, 3600).
-define(MINUTE_IN_SECONDS, 60).
-define(DATETIME_MAX_VALUE, {{9999, 31, 12}, {23, 59, 59}}).

parse("") ->
  throw(empty_string);
parse("* * * * *") ->
  {all, all, all, all, all};
parse(Expression) ->
  Fields = string:tokens(Expression, " "),
  parse_fields(Fields).

parse_fields(Fields) when length(Fields) == 5 ->
  FieldsZipped = lists:zip(Fields, [minute, hour, day, month, day_of_week]),
  [Field1, Field2, Field3, Field4, Field5] = [parse_field(FieldKind, Field) || {Field, FieldKind} <- FieldsZipped],
  {Field1, Field2, Field3, Field4, Field5};
parse_fields(_Fields) ->
  throw(invalid_fields_count).

get_field_spec(minute = _FieldName) ->
  {minute, 0, 59, []};
get_field_spec(hour = _FieldName) ->
  {hour, 0, 23, []};
get_field_spec(day = _FieldName) ->
  {day, 1, 31, []};
get_field_spec(month = _FieldName) ->
  {month, 1, 12, ["january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december"]};
get_field_spec(day_of_week = _FieldName) ->
  {day_of_week, 0, 6, ["sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday"]}.

parse_field(_, "*") ->
  all;
parse_field(FieldName, Field) ->
  FieldSpec = get_field_spec(FieldName),
  parse_field_impl(FieldSpec, Field).
parse_field_impl(FieldSpec, Field) ->
  CommaIndex = string:chr(Field, $,),
  if
    CommaIndex > 1 ->
      FieldValues = string:tokens(Field, ","),
      FieldValuesParsed = [parse_field_impl(FieldSpec, F) || F <- FieldValues],
      lists:append(FieldValuesParsed);
    true ->
      SlashIndex = string:chr(Field, $/),
      {Every, FieldValueStr} = if
                                 SlashIndex > 1 ->
                                   { parse_field_value(string:sub_string(Field, SlashIndex + 1), FieldSpec),
                                     string:sub_string(Field, 1, SlashIndex - 1) };
                                 true ->
                                   { 1, Field }
                               end,
      case FieldValueStr of
        "*" ->
          generate_field_values(-1, -1, Every, FieldSpec);
        _ ->
          DashIndex = string:chr(FieldValueStr, $-),
          if
            DashIndex > 1 ->
              First = parse_field_value(string:sub_string(FieldValueStr, 1, DashIndex - 1), FieldSpec),
              Last = parse_field_value(string:sub_string(FieldValueStr, DashIndex + 1), FieldSpec),
              generate_field_values(First, Last, Every, FieldSpec);
            true ->
              FieldValue = parse_field_value(FieldValueStr, FieldSpec),
              generate_field_values(FieldValue, Every, FieldSpec)
          end
      end
  end.

parse_field_value("", _FieldSpec) ->
  throw({bad_field, ""});
parse_field_value(FieldValue, {_, FieldMinValue, _, FieldNames} = _FieldSpec) ->
  case string:to_integer(FieldValue) of
    {Int, []} when Int >= 0 ->
      Int;
    _ when length(FieldNames) =< 0 ->
      throw({bad_field, FieldValue});
    _ ->
      case list_search(fun(FieldName) -> is_prefix(FieldName, string:to_lower(FieldValue)) end, FieldNames) of
        0 ->
          throw({bad_field, FieldValue});
        Index ->
          Index + FieldMinValue - 1
      end
  end.

generate_field_values(FieldValue, 1 = Interval, FieldSpec) ->
  generate_field_values(FieldValue, FieldValue, Interval, FieldSpec);
generate_field_values(FieldValue, Interval, {_, _, FieldMaxValue, _} = FieldSpec) ->
  generate_field_values(FieldValue, FieldMaxValue, Interval, FieldSpec).

generate_field_values(Start, End, Interval, {_, FieldMinValue, FieldMaxValue, _} = FieldSpec)
  when Start == End, Start < 0, Interval =< 1 ->
  generate_field_values(FieldMinValue, FieldMaxValue, 1, FieldSpec);
generate_field_values(Start, End, Interval, {_, FieldMinValue, FieldMaxValue, _} = FieldSpec)
  when Start == End, Start < 0 ->
  generate_field_values(FieldMinValue, FieldMaxValue, Interval, FieldSpec);
generate_field_values(Start, End, _Interval, {_, FieldMinValue, _, _} = _FieldSpec)
  when Start == End, Start >= 0, Start < FieldMinValue ->
  throw({bad_field, Start, lower_then, FieldMinValue});
generate_field_values(Start, End, _Interval, {_, _, FieldMaxValue, _} = _FieldSpec)
  when Start == End, Start >= 0, Start > FieldMaxValue ->
  throw({bad_field, Start, higher_then, FieldMaxValue});
generate_field_values(Start, End, Interval, FieldSpec)
  when Start > End ->
  generate_field_values(End, Start, Interval, FieldSpec);
generate_field_values(Start, End, Interval, {_, FieldMinValue, _, _} = FieldSpec)
  when Start < 0 ->
  generate_field_values(FieldMinValue, End, Interval, FieldSpec);
generate_field_values(Start, _End, _Interval, {_, FieldMinValue, _, _} = _FieldSpec)
  when Start < FieldMinValue ->
  throw({bad_field, Start, lower_then, FieldMinValue});
generate_field_values(Start, End, Interval, {_, _, FieldMaxValue, _} = FieldSpec)
  when End < 0 ->
  generate_field_values(Start, FieldMaxValue, Interval, FieldSpec);
generate_field_values(Start, End, _Interval, {_, _, FieldMaxValue, _} = _FieldSpec)
  when End > FieldMaxValue ->
  throw({bad_field, Start, higher_then, FieldMaxValue});
generate_field_values(Start, End, Interval, FieldSpec)
  when Interval < 1 ->
  generate_field_values(Start, End, 1, FieldSpec);
generate_field_values(Start, End, Interval, _FieldSpec) ->
  lists:seq(Start, End, Interval).

get_next_occurrences(Schedule, BaseDatetime, EndDateTime) ->
  get_next_occurrences(Schedule, BaseDatetime, EndDateTime, []).
get_next_occurrences(Schedule, BaseDatetime, EndDateTime, Acc)
  when BaseDatetime < EndDateTime ->
  Next = get_next_occurrence(Schedule, BaseDatetime, EndDateTime),
  get_next_occurrences(Schedule, Next, EndDateTime, Acc ++ [Next]);
get_next_occurrences(_Schedule, _BaseDatetime, _EndDateTime, Acc)  ->
  Acc.

get_next_occurrence_after_ms(Schedule, BaseDatetime) ->
  NextOccurrence = get_next_occurrence(Schedule, BaseDatetime),
  NowSeconds = calendar:datetime_to_gregorian_seconds(BaseDatetime),
  NextSeconds = calendar:datetime_to_gregorian_seconds(NextOccurrence),
  (NextSeconds - NowSeconds) * 1000.

get_next_occurrence(Schedule, BaseDatetime) ->
  get_next_occurrence(Schedule, BaseDatetime, ?DATETIME_MAX_VALUE).

get_next_occurrence(Schedule, BaseDatetime, EndDateTime) ->
  ScheduleList = erlang:tuple_to_list(Schedule),
  FieldsZipped = lists:zip(ScheduleList, [minute, hour, day, month, day_of_week]),
  ScheduleWithSpecList = [{get_field_spec(FieldName), FieldSchedule} || {FieldSchedule, FieldName} <- FieldsZipped],
  ScheduleWithSpec = erlang:list_to_tuple(ScheduleWithSpecList),
  get_next_occurrences_impl1(ScheduleWithSpec, BaseDatetime, EndDateTime).
get_next_occurrences_impl1(Schedule,
                           {BaseDate, {BaseHour, BaseMinute, _}} = BaseDatetime,
                           EndDateTime) ->
  get_next_occurrences_impl2(Schedule, BaseDatetime, EndDateTime, {BaseDate, {BaseHour, BaseMinute + 1, 0}}).

get_next_occurrences_impl2(Schedule, BaseDatetime, EndDatetime, Datetime) ->
  DatetimeMin = get_next_occurrences_impl2_min(Schedule, BaseDatetime, EndDatetime, Datetime),
  DatetimeHour = get_next_occurrences_impl2_hour(Schedule, BaseDatetime, EndDatetime, DatetimeMin),
  get_next_occurrences_impl3(Schedule, BaseDatetime, EndDatetime, DatetimeHour).
get_next_occurrences_impl3(Schedule, BaseDatetime, EndDatetime, Datetime) ->
  DatetimeDay = get_next_occurrences_impl2_day(Schedule, BaseDatetime, EndDatetime, Datetime),
  DatetimeMonth = get_next_occurrences_impl2_month(Schedule, BaseDatetime, EndDatetime, DatetimeDay),
  case check_month_day(Schedule, BaseDatetime, EndDatetime, DatetimeMonth) of
    true ->
      {{DatetimeMonthYear, DatetimeMonthMonth, _}, DatetimeMonthTime} = DatetimeMonth,
      DatetimeMonthChanged = {{DatetimeMonthYear, DatetimeMonthMonth, -1}, DatetimeMonthTime},
      get_next_occurrences_impl3(Schedule, BaseDatetime, EndDatetime, DatetimeMonthChanged);
    false ->
      get_next_occurrences_impl4(Schedule, BaseDatetime, EndDatetime, DatetimeMonth);
    end_date ->
      EndDatetime
  end.

get_next_occurrences_impl4(_Schedule, _BaseDatetime, EndDatetime, Datetime)
  when Datetime >= EndDatetime ->
  EndDatetime;
get_next_occurrences_impl4({_, _, _, _, DayOfWeekSpec} = Schedule,
                            _BaseDatetime, EndDatetime,
                            {Date, _} = Datetime) ->
  DayOfWeek = get_day_of_week(Date),
  case spec_contains(DayOfWeek, DayOfWeekSpec) of
    true ->
      Datetime;
    _ ->
      get_next_occurrences_impl1(Schedule, {Date, {23, 59, 0}}, EndDatetime)
  end.

get_next_occurrences_impl2_min({MinuteSpec, _, _, _, _} = _Schedule,
                               _BaseDatetime, _EndDateTime,
                               {Date, {Hour, Minute, _}} = _Datetime) ->
  NextMinute = next_value(Minute, MinuteSpec),
  case NextMinute of
    -1 ->
      {Date, {Hour + 1, first_value(MinuteSpec), 0}};
    _ ->
      {Date, {Hour, NextMinute, 0}}
  end.
get_next_occurrences_impl2_hour({MinuteSpec, HourSpec, _, _, _} = _Schedule,
                                {_, {BaseHour, _, _}} = _BaseDatetime,
                                _EndDateTime,
                                {{Year, Month, Day} = Date, {Hour, Minute, _}} = _Datetime) ->
  NextHour = next_value(Hour, HourSpec),
  if
    NextHour == -1 ->
      {{Year, Month, Day + 1}, {first_value(HourSpec), first_value(MinuteSpec), 0}};
    NextHour > BaseHour ->
      {{Year, Month, Day}, {NextHour, first_value(MinuteSpec), 0}};
    true ->
      {Date, {NextHour, Minute, 0}}
  end.
get_next_occurrences_impl2_day({MinuteSpec, HourSpec, DaySpec, _, _} = _Schedule,
                               {{_, _, BaseDay}, _} = _BaseDatetime,
                               _EndDateTime,
                               {{Year, Month, Day}, {Hour, Minute, _}} = _Datetime) ->
  NextDay =
  if
    Day == -1 ->
      Day;
    true ->
      next_value(Day, DaySpec)
  end,
  if
    NextDay == -1 ->
      {{Year, Month + 1, first_value(DaySpec)}, {first_value(HourSpec), first_value(MinuteSpec), 0}};
    NextDay > BaseDay ->
      {{Year, Month, NextDay}, {first_value(HourSpec), first_value(MinuteSpec), 0}};
    true ->
      {{Year, Month, NextDay}, {Hour, Minute, 0}}
  end.
get_next_occurrences_impl2_month({MinuteSpec, HourSpec, DaySpec, MonthSpec, _} = _Schedule,
                                 {{_, BaseMonth, _}, _} = _BaseDatetime,
                                 _EndDateTime,
                                 {{Year, Month, Day}, {Hour, Minute, _}} = _Datetime) ->
  NextMonth = next_value(Month, MonthSpec),
  if
    NextMonth ==  -1 ->
      {{Year + 1, first_value(MonthSpec), first_value(DaySpec)}, {first_value(HourSpec), first_value(MinuteSpec), 0}};
    NextMonth > BaseMonth ->
      {{Year, NextMonth, first_value(DaySpec)}, {first_value(HourSpec), first_value(MinuteSpec), 0}};
    true ->
      {{Year, NextMonth, Day}, {Hour, Minute, 0}}
  end.
check_month_day(_Schedule,
                {{BaseYear, BaseMonth, BaseDay}, _} = _BaseDatetime,
                {{EndYear, EndMonth, EndDay}, _} = _EndDateTime,
                {{Year, Month, Day}, _} = _Datetime)
  when Day > 28 andalso (Day /= BaseDay orelse Month /= BaseMonth orelse Year /= BaseYear) ->
  LastDay = calendar:last_day_of_the_month(Year, Month),
  if
    Day > LastDay ->
      if
        Year >= EndYear andalso Month >= EndMonth andalso Day >= EndDay ->
          end_date;
        true ->
          true
      end;
    true ->
      false
  end;
check_month_day(_Schedule, _BaseDatetime, _EndDateTime, _Datetime) ->
  false.

next_value(Start, {{_, FieldMinValue, _, _}, _} = _ValueSpec)
  when Start < FieldMinValue ->
  FieldMinValue;
next_value(Start, {{_, FieldMinValue, FieldMaxValue, _} = FieldSpec, all} = _ValueSpec) ->
  next_value(Start, {FieldSpec, lists:seq(FieldMinValue, FieldMaxValue)});
next_value(_Start, {_, []} = _ValueSpec) ->
  -1;
next_value(Start, {_, [Start | _T]} = _ValueSpec) ->
  Start;
next_value(Start, {_, [H | _T]} = _ValueSpec)
  when Start < H->
  H;
next_value(Start, {FieldSpec, FieldValues} = _ValueSpec) ->
  FieldValuesGreater = lists:dropwhile(fun(FieldValue) -> FieldValue < Start end, FieldValues),
  next_value(Start, {FieldSpec, FieldValuesGreater});
next_value(_Start, _ValueSpec) ->
  -1.

first_value({{_, FieldMinValue, _, _}, all} = _ValueSpec) ->
  FieldMinValue;
first_value({_, []} = ValueSpec) ->
  throw({bad_field, ValueSpec});
first_value({_, [H | _T]} = _ValueSpec) ->
  H.

list_search(Fun, List) ->
  list_search(Fun, 1, List).
list_search(_Fun, _IndexAcc, []) ->
  0;
list_search(Fun, IndexAcc, [H | T]) ->
  case Fun(H) of
    true ->
      IndexAcc;
    _ ->
      list_search(Fun, IndexAcc + 1, T)
  end.

is_prefix(Source, Prefix) ->
  string:str(Source, Prefix) == 1.

get_day_of_week(Date) ->
  case calendar:day_of_the_week(Date) of
    7 ->
      0;
    DayOfWeek ->
      DayOfWeek
  end.

spec_contains(_, {_, all}) ->
  true;
spec_contains(Value, {_, Values}) ->
  lists:member(Value, Values).