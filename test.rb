require 'rspec'
require './main'

describe "molecules" do
  it "reads the vars" do
    expect(molecules['a']).to be(71.04)
  end

  it "splits on the user input" do
    expect(parse_user_input("XZU")).to eq(%w(x z u))
  end

  it "splits with the number inputs" do
    expect(parse_user_input("A3L3TXZ")).to eq(%w(a3 l3 t x z))
  end

  # it "creates the molecules" do
  #   parsed_input = parse_user_input("
  #   expect(Molecule.new(parsed_input).weight).to eq(400)
  # end
end
