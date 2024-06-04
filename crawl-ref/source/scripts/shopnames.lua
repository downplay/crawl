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
