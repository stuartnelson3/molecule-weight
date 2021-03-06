require 'rspec'
require './main'

describe UserInput do
  let(:parser)               { UserInput }
  let(:expected_parsed_data) { %w(a c (3l) (3t) x z c*) }

  it "splits on the user input" do
    expect(parser.parse("XZU")).to eq(%w(x z u))
  end

  it "splits with the number inputs" do
    expect(parser.parse("AC(3L)(3T)XZC*")).to eq(expected_parsed_data)
  end

  it "handles spaces in the input" do
    expect(parser.parse("A C (3L) (3T) X Z C*")).to eq(expected_parsed_data)
  end

  it "differentiates between c and c*" do
    expect(parser.parse("AC*(3L)(3C*)XZ(C*3)")).to eq(%w(a c* (3l) (3c*) x z (c*3)))
  end

  it "parses reversed paren residues" do
    expect(parser.parse("A(L3)X(Z)")).to eq(%w(a (l3) x (z)))
  end
end

describe MassTypes do
  let(:bad_input) { ['garbage', 123] }
  let(:good_input) { ['monoisotopic', 'average'] }

  it 'accepts good input' do
    good_input.each do |i|
      expect(MassTypes.parse(i)).to eq(i.to_sym)
    end
  end

  it 'cleans bad input' do
    bad_input.each do |i|
      expect(MassTypes.parse(i)).to eq(:monoisotopic)
    end
  end
end

describe Peptide do
  let(:mass_type) { MassTypes.parse('monoisotopic') }
  let(:residues) { AminoAcids.new(mass_type).residues }
  let(:subject) { Peptide.new("AEF(3A)YXZ", residues) }

  it 'finds different masses based on mass type' do
    mono = AminoAcids.new(MassTypes.parse('monoisotopic')).residues
    avg = AminoAcids.new(MassTypes.parse('average')).residues
    seq = "AEF(3Q)YXZC*"
    mono_weight = Peptide.new(seq, mono).weight
    avg_weight = Peptide.new(seq, avg).weight
    expect(mono_weight).to_not eq (avg_weight)
  end

  it "calculates the molecule weight" do
    expect(subject.weight).to be_within(0.1).of(818.39 + 19)
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

  context "Acetylated peptides" do
    let(:mass_type) { MassTypes.parse('monoisotopic') }
    let(:residues) { AminoAcids.new(mass_type).residues }
    let(:subject)  { Peptide.new("Ac-AEF(3A)YXZ", residues) }

    it "correctly parses out acetylated petides" do
      expect(subject.molecules).to include("ac-")
    end

    it "correctly formats results that include Ac-" do
      weight = residues['ac-'] + residues['a'] + 19
      expect(subject.possible_sequences(weight, 5).keys).to include("Ac-A")
    end

    context NonTerminalAcetylateError do
      it "blows up on non-left-hand Ac- residue" do
        expect { Peptide.new("AEF(3A)YAc-XZ", residues) }.to raise_error NonTerminalAcetylateError
      end

      it "doesn't let you pass in multiple Ac- groups" do
        expect { Peptide.new("Ac-AEFAc-", residues) }.to raise_error NonTerminalAcetylateError
      end
    end
  end

  it "knows the weight of a combination" do
    expect(Peptide.new("XZU", residues).weight).to eq(308.18 + 19)
  end

  it "lists a sequence based on a molecular weight" do
    yxz_weight = 163.06 + 111.07 + 112.06 + 19
    expect(subject.possible_sequences(yxz_weight, 5)).to eq({ "YXZ" => yxz_weight.round(2) })
  end

  context "tolerance" do
    it "correctly uses tolerance" do
      # Missing +19 weight adjustment
      yxz_weight = 163.06 + 111.07 + 112.06
      expect(subject.possible_sequences(yxz_weight, 5)).to eq({ "No matches found" => yxz_weight.round(2) })

      expect(subject.possible_sequences(yxz_weight, 5+19)).to_not eq({ "No matches found" => yxz_weight.round(2) })
    end
  end

  it "notifies the user if there were no matches" do
    weight = 43532523354235
    expect(subject.possible_sequences(weight, 5)).to eq({ "No matches found" => weight })
  end

  context "end and non-end fragments" do
    let(:seq) { ['y','x','z','-nh2'] }
    let(:weight) { 163.06 + 111.07 + 112.06 }
    it "C-terminal amide adds 18 (ends with -NH2)" do
      expected_weight = weight + 18
      pep = Peptide.new("Ac-AEF(3A)YXZ-NH2", residues)
      expect(pep.calculate_weight(seq + ['-nh2'])).to eq(expected_weight.round(2))
    end

    it "N-terminal amines and C-terminal acids add 19" do
      expected_weight = weight + 19
      expect(subject.calculate_weight(seq)).to eq(expected_weight.round(2))
    end
  end

  context "AMINOACIDS" do
    it "should raise on a bad key" do
      expect { AminoAcids.new(mass_type).residues['NOMATCH'] }.to raise_error BadSequenceError
    end
  end

  context "wildcards" do
    let(:mass_type) { MassTypes.parse('monoisotopic') }
    let(:residues) { AminoAcids.new(mass_type).residues }


    it "should calculate correctly if a wildcard has been added" do
      wildcards = {
        "(1X)" => 100.5,
        "(2X)" => 110.3,
        "(3X)" => 120.7
      }

      with_wildcards = Peptide.new("XZU(1X)(2X)(3X)", residues, wildcards)
      expect(with_wildcards.weight).to eq(
        (Peptide.new("XZU", residues).weight + wildcards.values.inject(&:+)).round(2)
      )
    end

    it "should calculate correctly if weights are strings" do
      wildcards = {
        "(1X)" => "100.5",
        "(2X)" => "110.3",
        "(3X)" => "120.7"
      }
      with_wildcards = Peptide.new("XZU(1X)(2X)(3X)", residues, wildcards)
      expect(with_wildcards.weight).to eq(
        (Peptide.new("XZU", residues).weight + wildcards.values.map(&:to_f).inject(&:+)).round(2)
      )
    end

    it "should not add residues that have 0 for a value" do
      wildcards = {
        "(1X)" => "100.5",
        "(2X)" => "110.3",
        "(3X)" => ""
      }
      expect { Peptide.new("XZU(1X)(2X)(3X)", residues, wildcards) }.to raise_error BadSequenceError
    end

    it "should not overwrite existing residue weights" do
      wildcard = { "A" => 10 }
      original_weight = Peptide.new("A", residues).weight
      expect(Peptide.new("A", residues, wildcard).weight).to eq(original_weight)
    end
  end
end
