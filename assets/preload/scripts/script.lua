local hasDeathLine = false

local phase1 = {'ronald'}
local phase2 = {'ronald-lv9'}

function onGameOverMusicStart()



    for i = 0,#phase1 do 
        if getPropertyFromClass('PlayState', 'SONG.player2') == phase1[i+1] then 
            setSoundVolume('', 0.2)
            playSound('lose lines/phase1/lose'..getRandomInt(0,10), 1, 'deathLine')
        end
    end

    for i = 0,#phase2 do 
        if getPropertyFromClass('PlayState', 'SONG.player2') == phase2[i+1] then 
            setSoundVolume('', 0.2)
            playSound('lose lines/phase2/lose'..getRandomInt(11,21), 1, 'deathLine')
        end
    end
end
function onGameOverConfirm(a)
    stopSound('deathLine')
end
function onSoundFinished(tag)
    if tag == 'deathLine' then 
        soundFadeIn('', 4, 0.2, 1)
    end
end