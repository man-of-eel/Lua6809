This is an emulator for the Motorola 6809 in Lua. It is a port of the [usim](https://github.com/raybellis/usim) emulator, written in C++.

It is not a fast emulator but it should work with most things you throw at it—it is capable of emulating [Grant Searle's 6809 SBC](http://searle.x10host.com/6809/Simple6809.html) and complex BASIC programs.

It has only been tested under [Luau](https://github.com/luau-lang/luau) but it doesn't use much Luau-specific features, so it should probably be able to run under upstream Lua.

The emulator could do with a rewrite for performance & cleaner code but it's unlikely I'll get round to it so I've put it up anyway.

To use it, you could do something like:

```lua
local cpu = require(m6809)
local ram = table.create(65536,0) -- or use Luau's buffers
local map = {
  read = function(addr)
    return ram[addr+1]
  end,

  write = function(addr,data)
    ram[addr+1] = data
  end
}

cpu:set_addrmap(sys.map)
cpu:reset()

while true do
  cpu:tick()
  wait()
end

```
