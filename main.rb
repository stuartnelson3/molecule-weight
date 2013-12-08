AMINOACIDS = Hash.new {|hash, key|
  raise BadSequenceError, "Could not find weight for amino acid #{key.upcase}"
}

class BadSequenceError < StandardError; end

{
  'a'    => 71.04,
  'c'    => 103.01,
  'd'    => 115.03,
  'e'    => 129.04,
  'f'    => 147.07,
  'g'    => 57.02,
  'h'    => 137.06,
  'i'    => 113.08,
  'k'    => 128.09,
  'l'    => 113.08,
  'm'    => 131.04,
  'n'    => 114.04,
  'p'    => 97.05,
  'q'    => 128.06,
  'r'    => 156.1,
  's'    => 87.03,
  't'    => 101.05,
  'v'    => 99.07,
  'w'    => 186.08,
  'y'    => 163.06,
  '(3a)' => 85.06,
  '(3c)' => 117.03,
  '(3d)' => 129.05,
  '(3e)' => 143.06,
  '(3f)' => 161.09,
  '(3g)' => 71.04,
  '(3h)' => 151.08,
  '(3i)' => 127.1,
  '(3k)' => 142.11,
  '(3l)' => 127.1,
  '(3m)' => 145.06,
  '(3n)' => 128.06,
  '(3p)' => 111.07,
  '(3q)' => 142.07,
  '(3r)' => 170.12,
  '(3s)' => 101.05,
  '(3t)' => 115.07,
  '(3v)' => 113.09,
  '(3w)' => 200.1,
  '(3y)' => 117.08,
  'x'    => 111.07,
  'z'    => 112.06,
  'u'    => 85.05
}.each {|k,v| AMINOACIDS[k] = v }

class UserInput
  def self.parse input
    input.downcase.strip.scan(/\([0-9][a-z]\)|[a-z]/)
  end
end

class Peptide
  attr_reader :molecules, :weight, :combinations
  def initialize molecules, wildcards = {}
    add_wildcards wildcards
    @molecules = UserInput.parse molecules
    @weight = calculate_weight @molecules, :original
    @combinations = MoleculeCombinations.find_for @molecules
  end

  def add_wildcards wildcards
    wildcards.each do |k,v|
      AMINOACIDS[k.downcase] = v.to_f
    end
  end

  def calculate_weight molecules, original = false
    FragmentWeight.new(@molecules).calculate molecules, original
  end

  def combination_in_range combo, weight
    (calculate_weight(combo) - weight).abs <= 5
  end

  def possible_sequences weight
    matches = combinations.select {|combo|
      combination_in_range combo, weight
    }
    weights = matches.map {|m| calculate_weight(m) }
    matches.map!(&:join).map!(&:upcase) # ['a','b'] => 'AB'
    if h = Hash[matches.zip weights] and !h.empty?
      h # { "SEQUENCE" => weight }
    else
      { "No matches found" => weight }
    end
  end
end

class MoleculeCombinations
  def self.find_for molecules
    a = []
    (1..molecules.length).each {|i| a+=molecules.each_cons(i).to_a }
    a
  end
end

class FragmentWeight
  def initialize molecules
    @molecules = molecules
  end

  def calculate molecules, original = false
    weight_adjustment = calculate_adjustment molecules, original
    weights = molecules.map {|m| AMINOACIDS[m] }
    weights.inject(&:+).round(1) + weight_adjustment
  end

  def calculate_adjustment molecules, original
    if original || end_of_fragment?(molecules)
      18
    else
      19
    end
  end

  def end_of_fragment? molecules
    @molecules[-molecules.length, molecules.length] == molecules
  end
end
