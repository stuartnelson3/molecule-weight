require './amino_acids'

class BadSequenceError < StandardError; end
class NonTerminalAcetylateError < StandardError; end
class UnrecognizedMassType < StandardError; end

class MassTypes
  TYPES = {'monoisotopic' => :monoisotopic.freeze,
           'average' => :average.freeze }

  TYPES.default = TYPES['monoisotopic']
  def self.parse input
    TYPES[input]
  end
end

class UserInput
  def self.parse input
    input.downcase.strip.scan(/ac-|-nh2|\([0-9]?[a-z]\*?\)|[a-z]\*?|\([a-z]\*?[0-9]\)/)
  end
end

class Peptide
  attr_reader :molecules, :weight, :combinations, :residues
  def initialize molecules, residues, wildcards = {}
    @residues = residues
    add_wildcards wildcards
    @molecules = parse_molecules molecules
    @weight = calculate_weight @molecules
    @combinations = MoleculeCombinations.find_for @molecules
  end

  def parse_molecules molecules
    parsed_molecules = UserInput.parse molecules
    if non_terminal_acetylate? parsed_molecules
      raise NonTerminalAcetylateError, "Error: Acetylation in a non-terminal position."
    end
    parsed_molecules
  end

  def non_terminal_acetylate? parsed_molecules
    parsed_molecules.include?("ac-") &&
      (parsed_molecules.first != "ac-" ||
       parsed_molecules.select {|pm| pm == "ac-" }.count > 1)
  end

  def add_wildcards wildcards
    wildcards.each do |k,v|
      if !@residues.fetch(k.downcase) { nil } && v = v.to_f
        @residues[k.downcase] = v unless v.zero?
      end
    end
  end

  def calculate_weight molecules
    FragmentWeight.new(@molecules, @residues).calculate molecules
  end

  def combination_in_range combo, weight, tolerance
    (calculate_weight(combo) - weight).abs <= tolerance
  end

  def possible_sequences weight, tolerance
    matches = combinations.select {|combo|
      combination_in_range combo, weight, tolerance
    }
    weights = matches.map {|m| calculate_weight(m) }
    matches.map!(&:join).map! {|seq| seq.upcase.gsub(/AC-/,"Ac-") }
    if h = Hash[matches.zip weights] and !h.empty?
      h # { "SEQUENCE" => weight }
    else
      { "No matches found" => weight }
    end
  end
end

class MoleculeCombinations
  def self.find_for molecules
    (1..molecules.length).flat_map {|i| molecules.each_cons(i).to_a }
  end
end

class FragmentWeight
  def initialize molecules, residues
    @molecules = molecules
    @residues = residues
  end

  def calculate molecules
    weight_adjustment = calculate_adjustment molecules
    weights = molecules.map {|m| @residues[m] }
    weights.inject(&:+).round(2) + weight_adjustment
  end

  def calculate_adjustment molecules
    # Check for -NH2
    if c_terminal_amide?(molecules)
      18
    else
      19
    end
  end

  def c_terminal_amide? molecules
    @molecules.last == '-nh2'
  end
end
