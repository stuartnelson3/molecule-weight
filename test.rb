require 'rspec'
require './main'

describe UserInput do
  let(:parser)               { UserInput }
  let(:expected_parsed_data) { %w(a (3l) (3t) x z) }

  it "splits on the user input" do
    expect(parser.parse("XZU")).to eq(%w(x z u))
  end

  it "splits with the number inputs" do
    expect(parser.parse("A(3L)(3T)XZ")).to eq(expected_parsed_data)
  end

  it "handles spaces in the input" do
    expect(parser.parse("A (3L) (3T) X Z")).to eq(expected_parsed_data)
  end

end

describe Peptide do
  let(:subject) { Peptide.new("AEF(3A)YXZ") }

  it "calculates the molecule weight" do
    expect(subject.weight).to be_within(0.1).of(818.39 + 18)
  end

  it "knows the possible combinations of the molecules" do
    expect(subject.combinations).to eq(
      [
        ["a"], ["e"], ["f"], ["(3a)"], ["y"], ["x"], ["z"],
        ["a", "e"], ["e", "f"], ["f", "(3a)"], ["(3a)", "y"],
        ["y", "x"], ["x", "z"], ["a", "e", "f"], ["e", "f", "(3a)"],
        ["f", "(3a)", "y"], ["(3a)", "y", "x"], ["y", "x", "z"],
        ["a", "e", "f", "(3a)"], ["e", "f", "(3a)", "y"],
        ["f", "(3a)", "y", "x"], ["(3a)", "y", "x", "z"],
        ["a", "e", "f", "(3a)", "y"], ["e", "f", "(3a)", "y", "x"],
        ["f", "(3a)", "y", "x", "z"], ["a", "e", "f", "(3a)", "y", "x"],
        ["e", "f", "(3a)", "y", "x", "z"], ["a", "e", "f", "(3a)", "y", "x", "z"]
      ]
    )
  end

  it "knows the weight of a combination" do
    expect(Peptide.new("XZU").weight).to eq(308.2 + 18)
  end

  it "lists a sequence based on a molecular weight" do
    yxz_weight = 163.06 + 111.07 + 112.06 + 18
    expect(subject.possible_sequences(yxz_weight)).to eq({ "YXZ" => yxz_weight.round(1) })
  end

  it "notifies the user if there were no matches" do
    weight = 43532523354235
    expect(subject.possible_sequences(weight)).to eq({ "No matches found" => weight })
  end

  context "end and non-end fragments" do
    it "adds 18 to end fragments" do
      expected_weight = 163.06 + 111.07 + 112.06 + 18
      expect(subject.calculate_weight(['y','x','z'])).to eq(expected_weight.round(1))
    end

    it "adds 19 to non-ending fragments" do
      expected_weight = 71.04 + 129.04 + 147.07 + 19
      expect(subject.calculate_weight(['a','e','f'])).to eq(expected_weight.round(1))
    end
  end

  context "AMINOACIDS" do
    it "should raise on a bad key" do
      expect { AminoAcids.new.residues['NOMATCH'] }.to raise_error BadSequenceError
    end
  end

  context "wildcards" do
    it "should calculate correctly if a wildcard has been added" do
      wildcards = {
        "(1X)" => 100.5,
        "(2X)" => 110.3,
        "(3X)" => 120.7
      }
      with_wildcards = Peptide.new("XZU(1X)(2X)(3X)", wildcards)
      expect(with_wildcards.weight).to eq(
        Peptide.new("XZU").weight + wildcards.values.inject(&:+)
      )
    end

    it "should calculate correctly if weights are strings" do
      wildcards = {
        "(1X)" => "100.5",
        "(2X)" => "110.3",
        "(3X)" => "120.7"
      }
      with_wildcards = Peptide.new("XZU(1X)(2X)(3X)", wildcards)
      expect(with_wildcards.weight).to eq(
        Peptide.new("XZU").weight + wildcards.values.map(&:to_f).inject(&:+)
      )
    end

    it "should not add residues that have 0 for a value" do
      wildcards = {
        "(1X)" => "100.5",
        "(2X)" => "110.3",
        "(3X)" => ""
      }
      expect { Peptide.new("XZU(1X)(2X)(3X)", wildcards) }.to raise_error BadSequenceError
    end
  end
end
