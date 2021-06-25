defmodule PollutionData do
  def parseDate str do
    str
    |> String.split("-")
    |> Enum.reverse
    |> Enum.map(&Integer.parse/1)
    |> Enum.map(fn ({int, _}) -> int end)
    |> :erlang.list_to_tuple
  end

  def parseTime str do
    str
    |> String.split(":")
    |> Enum.map(&Integer.parse/1)
    |> Enum.map(fn ({int, _}) -> int end)
    |> :erlang.list_to_tuple
  end

  def parseFloat str do
    {f, _} = Float.parse(str)
    f
  end

  def parse str do
    [date, time, x, y, value] = String.split(str, ",")

    %{:datetime => {parseDate(date), parseTime(time <> ":0")}, :location => {parseFloat(x), parseFloat(y)}, :pollutionLevel => parseFloat(value)}
  end

  def importLinesFromCSV filename do
    File.read!(filename) |>
    String.split("\r\n") |>
    Enum.map(&PollutionData.parse/1)
  end

  def identifyStations data do
    data
    |> Enum.map(fn (record) -> record[:location] end)
    |> Enum.uniq
    |> Enum.map(fn (coords) ->
    {x, y} = coords
    {"station_#{x}_#{y}", coords} end)
  end

  def addStation station do
    {name, coords} = station
    :pollution_gen_client.addStation(name, coords)
  end

  def addValue data do
    :pollution_gen_client.addValue(data[:location], data[:datetime], "PM10", data[:pollutionLevel])
  end

  def loadData filename do
    data = importLinesFromCSV filename
    data
    |> identifyStations
    |> Enum.each &addStation

    data
    |> Enum.each &addValue
  end
end
