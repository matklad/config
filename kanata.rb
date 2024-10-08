class Kanata < Formula
  desc "A cross-platform software keyboard remapper for Linux, macOS and Windows."
  homepage "https://github.com/jtroo/kanata"
  url "https://github.com/jtroo/kanata/archive/refs/tags/v1.7.0-prerelease-1.tar.gz"
  sha256 "f0e2ac2717c79b90d2a044257b1a13f8c734b0823458f00b123f8c267466e0c7"
  license "LGPL-3.0-only"

  depends_on "rust" => :build

  def install
    system "cargo", "install", *std_cargo_args
  end

  test do
    system "#{bin}/kanata", "--version"
  end
end
