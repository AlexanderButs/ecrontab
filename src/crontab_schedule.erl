-module(crontab_schedule).
-author("alexb").

%% API
-export([
  parse/1,
  get_next_occurrence/2,
  get_next_occurrence_after_ms/2
]).

-define(DAY_IN_SECONDS, 86400).
-define(HOUR_IN_SECONDS, 3600).
-define(MINUTE_IN_SECONDS, 60).

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

parse_field(_, "*") ->
  all;
parse_field(minute, Field) ->
  parse_field({minute, 0, 59, []}, Field);
parse_field(hour, Field) ->
  parse_field({hour, 0, 23, []}, Field);
parse_field(day, Field) ->
  parse_field({day, 1, 31, []}, Field);
parse_field(month, Field) ->
  parse_field({month, 1, 12, ["january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december"]}, Field);
parse_field(day_of_week, Field) ->
  parse_field({day_of_week, 0, 6, ["sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday"]}, Field);
parse_field(FieldSpec, Field) ->
  CommaIndex = string:chr(Field, $,),
  if
    CommaIndex > 1 ->
      FieldValues = string:tokens(Field, ","),
      FieldValuesParsed = [parse_field(FieldSpec, F) || F <- FieldValues],
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

get_next_occurrence_after_ms(Schedule, Now) ->
  NextOccurrence = get_next_occurrence(Schedule, Now),
  NowSeconds = calendar:datetime_to_gregorian_seconds(Now),
  NextSeconds = calendar:datetime_to_gregorian_seconds(NextOccurrence),
  (NextSeconds - NowSeconds) * 1000.

get_next_occurrence(Schedule, Now) ->
  DateTime1 = advance_seconds(Now, ?MINUTE_IN_SECONDS),
  {{Y, Mo, D}, {H, M, _}} = DateTime1,
  DateTime2 = {{Y, Mo, D}, {H, M, 0}},
  get_next_occurrence(not_done, Schedule, DateTime2).

get_next_occurrence(done, _, DateTime) ->
  DateTime;
get_next_occurrence(not_done, Schedule, DateTime) ->
  {MinuteSpec, HourSpec, DayOfMonthSpec, MonthSpec, DayOfWeekSpec} =
    Schedule,
  {{Year, Month, Day},  {Hour, Minute, _}} = DateTime,
  {Done, Time} =
    case value_valid(MonthSpec, 1, 12, Month) of
      false ->
        case Month of
          12 ->
            {not_done, {{Year + 1, 1, 1}, {0, 0, 0}}};
          Month ->
            {not_done, {{Year, Month + 1, 1}, {0, 0, 0}}}
        end;
      true ->
        DayOfWeek = case calendar:day_of_the_week(Year, Month, Day) of
                      7 ->
                        0; % we want 0 to be Sunday not 7
                      DOW ->
                        DOW
                    end,
        DOMValid = value_valid(DayOfMonthSpec, 1, 31, Day),
        DOWValid = value_valid(DayOfWeekSpec, 0, 6, DayOfWeek),
        case (((DayOfMonthSpec /= all) and
               (DayOfWeekSpec /= all) and
               (DOMValid and DOWValid)) or (DOMValid and DOWValid)) of
          false ->
            Temp1 = advance_seconds(DateTime, ?DAY_IN_SECONDS),
            {{Y, M, D}, {_, _, _}} = Temp1,
            {not_done, {{Y, M, D}, {0, 0, 0}}};
          true ->
            case value_valid(HourSpec, 0, 23, Hour) of
              false ->
                Temp3 = advance_seconds(DateTime,
                  ?HOUR_IN_SECONDS),
                {{Y, M, D}, {H, _, _}} = Temp3,
                {not_done, {{Y, M, D}, {H, 0, 0}}};
              true ->
                case value_valid(
                  MinuteSpec, 0, 59, Minute) of
                  false ->
                    {not_done, advance_seconds(
                      DateTime,
                      ?MINUTE_IN_SECONDS)};
                  true ->
                    {done, DateTime}
                end
            end
        end
    end,
  get_next_occurrence(Done, Schedule, Time).

value_valid(Spec, Min, Max, Value) when Value >= Min, Value =< Max->
  case Spec of
    all ->
      true;
    Spec ->
      ValidValues = extract_integers(Spec, Min, Max),
      lists:any(fun(Item) ->
        Item == Value
      end, ValidValues)
  end.

advance_seconds(DateTime, Seconds) ->
  Seconds1 = calendar:datetime_to_gregorian_seconds(DateTime) + Seconds,
  calendar:gregorian_seconds_to_datetime(Seconds1).

extract_integers(Spec, Min, Max) when Min < Max ->
  extract_integers(Spec, Min, Max, []).

extract_integers([], Min, Max, Acc) ->
  Integers = lists:sort(sets:to_list(sets:from_list(lists:flatten(Acc)))),
  lists:foreach(fun(Int) ->
    if
      Int < Min ->
        throw({error, {out_of_range, {min, Min},
          {value, Int}}});
      Int > Max ->
        throw({error, {out_of_range, {max, Max},
          {value, Int}}});
      true ->
        ok
    end
  end, Integers),
  Integers;
extract_integers(Spec, Min, Max, Acc) ->
  [H|T] = Spec,
  Values = case H of
             Integer when is_integer(Integer) ->
               [Integer]
           end,
  extract_integers(T, Min, Max, [Values|Acc]).

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
