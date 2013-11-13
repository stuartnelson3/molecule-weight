require 'rspec'
require './main'

describe "molecules" do
  it "splits on the user input" do
    expect(parse_user_input("XZU")).to eq(%w(x z u))
  end

  it "splits with the number inputs" do
    expect(parse_user_input("A3L3TXZ")).to eq(%w(a 3l 3t x z))
  end

end

describe AminoAcid do
  let(:parsed_input) { parse_user_input("AEF3AYXZ") }
  let(:subject)      { AminoAcid.new(parsed_input) }

  it "calculates the molecule weight" do
    expect(subject.weight).to be_within(0.1).of(818.39)
  end

  it "knows the possible combinations of the molecules" do
    expect(AminoAcid.new(parsed_input).combinations).to eq(
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
end

