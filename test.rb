require 'rspec'
require './main'

describe UserInput do
  let(:parser) { UserInput }
  it "splits on the user input" do
    expect(parser.parse("XZU")).to eq(%w(x z u))
  end

  it "splits with the number inputs" do
    expect(parser.parse("A3L3TXZ")).to eq(%w(a 3l 3t x z))
  end

  it "handles spaces in the input" do
    expect(parser.parse("A 3L 3T X Z")).to eq(%w(a 3l 3t x z))
  end

end

describe AminoAcid do
  let(:subject) { AminoAcid.new("AEF3AYXZ") }

  it "calculates the molecule weight" do
    expect(subject.weight).to be_within(0.1).of(818.39)
  end

  it "knows the possible combinations of the molecules" do
    expect(subject.combinations).to eq(
      [
        ["a"], ["e"], ["f"], ["3a"], ["y"], ["x"], ["z"],
        ["a", "e"], ["e", "f"], ["f", "3a"], ["3a", "y"],
        ["y", "x"], ["x", "z"], ["a", "e", "f"], ["e", "f", "3a"],
        ["f", "3a", "y"], ["3a", "y", "x"], ["y", "x", "z"],
        ["a", "e", "f", "3a"], ["e", "f", "3a", "y"],
        ["f", "3a", "y", "x"], ["3a", "y", "x", "z"],
        ["a", "e", "f", "3a", "y"], ["e", "f", "3a", "y", "x"],
        ["f", "3a", "y", "x", "z"], ["a", "e", "f", "3a", "y", "x"],
        ["e", "f", "3a", "y", "x", "z"], ["a", "e", "f", "3a", "y", "x", "z"]
      ]
    )
  end

  it "knows the weight of a combination" do
    expect(AminoAcid.new("XZU").weight).to eq(308.18)
  end

  it "lists a sequence based on a molecular weight" do
    yxz_weight = 163.06 + 111.07 + 112.06
    expect(subject.possible_sequences(yxz_weight)).to eq(["YXZ"])
  end
end

