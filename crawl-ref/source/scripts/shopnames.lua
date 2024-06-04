-- Generates a huge number of shop names to test the generator
-- Runs through the full range of "levels" (we increment 2 levels per dungeon
-- level, so 54), all shop types, and again with Gozag shops
-- Usage:
-- ./crawl -script shopnames 1> shopnames_test.txt

local shop_types = {
    "general",
    "antiques",
    "weapon",
    "antique weapon",
    "armour",
    "antique armour",
    "book",
    "scroll",
    "distillery",
    "jewellery",
    "gadget"
}

for i,shop in ipairs(shop_types) do
    print (shop .. ":")
    for level = 1,54 do
        print (level .. ": " .. dgn.shopname(shop, level))
    end
end

print ("Gozag!")

for i,shop in ipairs(shop_types) do
    print (shop .. ":")
    for level = 1,54 do
        print (level .. ": " .. dgn.shopname(shop, level, true))
    end
end
