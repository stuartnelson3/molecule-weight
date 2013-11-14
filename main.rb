require 'pp'

MOLECULES =
  {
    'a' => 71.04,
    'c' => 103.01,
    'd' => 115.03,
    'e' => 129.04,
    'f' => 147.07,
    'g' => 57.02,
    'h' => 137.06,
    'i' => 113.08,
    'k' => 128.09,
    'l' => 113.08,
    'm' => 131.04,
    'n' => 114.04,
    'p' => 97.05,
    'q' => 128.06,
    'r' => 156.1,
    's' => 87.03,
    't' => 101.05,
    'v' => 99.07,
    'w' => 186.08,
    'y' => 163.06,
    '3a' => 85.06,
    '3c' => 117.03,
    '3d' => 129.05,
    '3e' => 143.06,
    '3f' => 161.09,
    '3g' => 71.04,
    '3h' => 151.08,
    '3i' => 127.1,
    '3k' => 142.11,
    '3l' => 127.1,
    '3n' => 128.06,
    '3p' => 111.07,
    '3q' => 142.07,
    '3r' => 170.12,
    '3s' => 101.05,
    '3t' => 115.07,
    '3v' => 113.09,
    '3w' => 200.1,
    '3y' => 117.08,
    'x' => 111.07,
    'z' => 112.06,
    'u' => 85.05
  }

def main
  molecules = {
    X: 43.2,
    Y: 84.2,
    Z: 28.5,
    P: 73.02,
    D: 29
  }

  result = {}

  (1..molecules.length).to_a.each do |len|
    array_of_keys = molecules.keys.each_cons(len).to_a
    array_of_values = array_of_keys.map {|keys|
      keys.map {|k| molecules[k] }
    }
    array_of_keys.each_with_index do |key, i|
      result[key] = array_of_values[i].inject(&:+)
    end
  end
  result
end
# pp main

class UserInput
  def self.parse input
    input.downcase.strip.scan(/[0-9]?[a-z]/)
  end
end

class AminoAcid
  attr_reader :molecules, :weight, :combinations
  def initialize molecules
    @molecules = UserInput.parse molecules
    @weight = calculate_weight @molecules
    @combinations = find_combinations @molecules
  end

  def calculate_weight molecules
    # add 19 for H, OH, and cation
    molecules.map {|m| MOLECULES[m] }.compact.inject(&:+).round(1) + 19
  end

  def find_combinations molecules
    a = []
    (1..molecules.length).to_a.each {|i| a+=molecules.each_cons(i).to_a }
    a
  end

  def possible_sequences weight
    matches = combinations.select {|combo|
      calculate_weight(combo)+5 > weight &&
        calculate_weight(combo)-5 < weight
    }
    weights = matches.map {|m| calculate_weight(m) }
    matches.map!(&:join).map!(&:upcase) # ['a','b'] => 'AB'
    Hash[matches.zip weights] # { "SEQUENCE" => weight }
  end
end
