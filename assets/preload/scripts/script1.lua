--all of this was hidden in the first demo build lol
local mugenHudSongs = {
    'the idea erect', 'combo meal erect', 'REDACTED', 'deaf to all but the ronald', 'REDACTED'
}
local characterMap = {
    {'REDACTED', 'REDACTED'},
    {'bf', 'Boyfriend'},
    {'mc-bf', 'Boyfriend'},
    {'REDACTED', 'REDACTED'},
    {'touhou_bf', 'Boyfriend'},
    {'ronald', 'Ronald'},
    {'touhou_donald', 'Ronald'},
    {'ronald-lv9', 'Ronald Lv9'},
    {'REDACTED', 'REDACTED'},
    {'REDACTED', 'REDACTED'},
}
function onCreate()
    local songLower = string.lower(songName)
    for i = 0,#mugenHudSongs-1 do 
        if mugenHudSongs[i+1] == songLower then 
            setProperty('hudStyle', 'MUGEN')
        end
    end
end
function onCreatePost()
    local p1Name = ''
    local p2Name = ''
    for i = 0,#characterMap-1 do 
        if characterMap[i+1][1] == dadName then 
            p2Name = characterMap[i+1][2]
        end
        if characterMap[i+1][1] == boyfriendName then 
            p1Name = characterMap[i+1][2]
        end
    end
    local yVal = getProperty('healthBarBG.y')-28
    if getProperty('hudStyle') == 'MUGEN' then 
        makeLuaText('p2Name', p2Name, 0, 150, yVal)
        makeLuaText('p1Name', p1Name, 0, screenWidth-160, yVal)


        setTextFont('p1Name', 'RoA Small.ttf')
        setProperty('p1Name.borderSize', 2.5)
        setProperty('p1Name.antialiasing', false)

        setTextFont('p2Name', 'RoA Small.ttf')
        setProperty('p2Name.borderSize', 2.5)
        setProperty('p2Name.antialiasing', false)

        setTextSize('p1Name', 24)
        setTextSize('p2Name', 24)
        setTextAlignment('p2Name', 'left')
        setTextAlignment('p1Name', 'right')
        setProperty('p1Name.x', getProperty('p1Name.x')-getProperty('p1Name.width'))
        addLuaText('p1Name')
        addLuaText('p2Name')
    end
end