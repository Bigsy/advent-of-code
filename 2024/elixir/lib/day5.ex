defmodule Day5 do
  def parse_rule(rule) do
    [before_val, after_val] = String.split(rule, "|")
    {String.to_integer(before_val), String.to_integer(after_val)}
  end

  def parse_update(update) do
    update
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
  end

  def parse_input(input) do
    [rules_section, updates_section] = String.split(input, "\n\n")

    rules = rules_section
    |> String.split("\n")
    |> Enum.map(&parse_rule/1)

    updates = updates_section
    |> String.split("\n")
    |> Enum.map(&parse_update/1)

    %{rules: rules, updates: updates}
  end

  def valid_order?(rules, pages) do
    page_indices = pages
    |> Enum.with_index()
    |> Map.new(fn {page, idx} -> {page, idx} end)

    Enum.all?(rules, fn {before_val, after_val} ->
      !Map.has_key?(page_indices, before_val) ||
      !Map.has_key?(page_indices, after_val) ||
      Map.get(page_indices, before_val) < Map.get(page_indices, after_val)
    end)
  end

  def middle_number(nums) do
    Enum.at(nums, div(length(nums), 2))
  end

  def swap_positions(pages, {before_val, after_val}) do
    before_idx = Enum.find_index(pages, &(&1 == before_val))
    after_idx = Enum.find_index(pages, &(&1 == after_val))

    if before_idx != nil and after_idx != nil and before_idx > after_idx do
      List.replace_at(
        List.replace_at(pages, after_idx, before_val),
        before_idx,
        after_val
      )
    else
      pages
    end
  end

  def sort_by_rules(rules, pages) do
    page_set = MapSet.new(pages)
    relevant_rules = Enum.filter(rules, fn {before_val, after_val} ->
      MapSet.member?(page_set, before_val) and MapSet.member?(page_set, after_val)
    end)

    do_sort_by_rules(Enum.to_list(pages), relevant_rules)
  end

  defp do_sort_by_rules(current_pages, relevant_rules) do
    next_pages = Enum.reduce(relevant_rules, current_pages, &swap_positions(&2, &1))
    if next_pages == current_pages do
      current_pages
    else
      do_sort_by_rules(next_pages, relevant_rules)
    end
  end

  def solve_part1(input) do
    %{rules: rules, updates: updates} = parse_input(input)

    updates
    |> Enum.filter(&valid_order?(rules, &1))
    |> Enum.map(&middle_number/1)
    |> Enum.sum()
  end

  def solve_part2(input) do
    %{rules: rules, updates: updates} = parse_input(input)

    updates
    |> Enum.reject(&valid_order?(rules, &1))
    |> Enum.map(&(sort_by_rules(rules, &1) |> middle_number()))
    |> Enum.sum()
  end
end
