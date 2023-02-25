



function onCreate()
    --addLuaScript('SimpleModchartTemplate') -- load the script
    --triggerEvent('legacyPsychMode') 


end

local scrollSwitch = 520 --can be useful for when changing y values to work with both scrolls
--example, want to move note y to center of the screen, use the value scrollSwitch/2 and it should work both upscroll and downscroll 
function onCreatePost()
    if downscroll then 
		scrollSwitch = -520
	end

    --setProperty('incomingAngle.x', 90)

    --setProperty('playerNotePathAlpha.alpha', 0.75)
    if getPropertyFromClass('ClientPrefs', 'modcharts') then 
        runHaxeCode([[
            game.playfieldRenderer = new PlayfieldRenderer(game.strumLineNotes, game.notes, game);
            game.playfieldRenderer.cameras = [game.camHUD];
            game.add(game.playfieldRenderer);
            game.remove(game.grpNoteSplashes);
            game.add(game.grpNoteSplashes);
        ]])

        removeLuaSprite('theLimit', false)
        removeLuaText('limitCounter', false)
        addLuaSprite('theLimit',true);
        addLuaText('limitCounter');
    end


end

function onSongStart()--for step 0
    --introBuzz()
    --triggerEvent('setTimeStop', '54180', '93500')

   
end

local mainMelody = {0, 4,6,8,12, 16, 20, 22, 24, 28, 32, 36, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60, 62}
local longNotes = {0,8,12,16,24,28,32,40}

local woap = {192,704, 960, 1216, 1600, 1856}

local startMoveThing = 0
local waveThing = 0

local sectionsToNotDoMelodyThing = {0,1,2,3,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,58,59,92,93,94,95,96,97,98,99,132,133,134,135,136,137,138,139,147,148,149,150}
local swapThing = 1

function onStepHit()
    --[[
    local doMelodyThing = true
    for i = 0,#sectionsToNotDoMelodyThing-1 do 
        if math.floor(curStep/16) == sectionsToNotDoMelodyThing[i+1] then 
            doMelodyThing = false
        end
    end
    if doMelodyThing then 
        for i = 0,#mainMelody-1 do --reads a list of steps from the array
            if curStep % 64 == mainMelody[i+1] then 
                local played = false
                for j = 0,#longNotes-1 do 
                    if curStep % 64 == longNotes[j+1] then
                        setProperty('tipsy.y', 0.5)
                        doTweenY('tipsy', 'tipsy', 0, stepCrochet/500, 'backInOut')
                        swapThing = swapThing * -1
                        played = true
                    end
                end
    
                if curStep % 64 >= 48 then 
                    setProperty('strumOffset'..waveThing..'.y', -20)
                    setProperty('strumOffset'..(waveThing+4)..'.y', -20)
                    if waveThing == 0 then 
                        setProperty('strumOffset'..(3)..'.y', 0)
                        setProperty('strumOffset'..(7)..'.y', 0)
                    else 
                        setProperty('strumOffset'..(waveThing-1)..'.y', 0)
                        setProperty('strumOffset'..(waveThing-1+4)..'.y', 0)
                    end
                    played = true
                    waveThing = (waveThing + 1) % 4
                end
                if not played then 
                    setProperty('scale.x', 1)
                    doTweenX('scaleX', 'scale', 0.7, stepCrochet/500, 'backInOut')       
                end
                --0triggerEvent('Add Camera Zoom', 0.005, 0.005)
            end
        end
    end
    
    if curStep % 64 == 0 then 
        for j = 0,7 do 
            setProperty('strumOffset'..j..'.y', 0)
        end
    end


    introBuzz()


    if curStep == 192 then 
        for i = 0,7 do 
            doTweenAngle('strumOffset'..i, 'strumOffset'..i, 0, (stepCrochet/1000)*3, 'cubeInOut')
            doTweenAngle('confusion'..i, 'confusion'..i, 0, (stepCrochet/1000)*3, 'cubeInOut')
        end
    elseif curStep == 704 then 
        doTweenAngle('globalStrumOffset', 'globalStrumOffset', -200, (stepCrochet/1000)*3, 'cubeInOut') 
    elseif curStep == 960 then 
        doTweenAngle('globalStrumOffset', 'globalStrumOffset', 0, (stepCrochet/1000)*3, 'cubeInOut') 

    elseif curStep == 832 then 
        doTweenX('speen', 'noteRot', 180, (stepCrochet/1000)*6, 'cubeInOut')
        local time = (stepCrochet/1000)*6
        local ease = 'cubeInOut'
        doTweenX('0x', 'strumOffset0', -112 * 3, time, ease)
        doTweenX('1x', 'strumOffset1', -112 * 1, time, ease)
        doTweenX('2x', 'strumOffset2', -112 * -1, time, ease)
        doTweenX('3x', 'strumOffset3', -112 * -3, time, ease)
        doTweenX('4x', 'strumOffset4', -112 * 3, time, ease)
        doTweenX('5x', 'strumOffset5', -112 * 1, time, ease)
        doTweenX('6x', 'strumOffset6', -112 * -1, time, ease)
        doTweenX('7x', 'strumOffset7', -112 * -3, time, ease) 
        --triggerEvent('flip',''..(stepCrochet/1000)*3, 'cubeInOut')
    elseif curStep == 864 then 
        doTweenX('speen', 'noteRot', 360, (stepCrochet/1000)*6, 'cubeInOut') 
        triggerEvent('resetX',''..(stepCrochet/1000)*6, 'cubeInOut')
    elseif curStep == 832+64 then 
        doTweenX('speen', 'noteRot', 180, (stepCrochet/1000)*6, 'cubeInOut') 
        --triggerEvent('flip',''..(stepCrochet/1000)*3, 'cubeInOut')
        local time = (stepCrochet/1000)*6
        local ease = 'cubeInOut'
        doTweenX('0x', 'strumOffset0', -112 * 3, time, ease)
        doTweenX('1x', 'strumOffset1', -112 * 1, time, ease)
        doTweenX('2x', 'strumOffset2', -112 * -1, time, ease)
        doTweenX('3x', 'strumOffset3', -112 * -3, time, ease)
        doTweenX('4x', 'strumOffset4', -112 * 3, time, ease)
        doTweenX('5x', 'strumOffset5', -112 * 1, time, ease)
        doTweenX('6x', 'strumOffset6', -112 * -1, time, ease)
        doTweenX('7x', 'strumOffset7', -112 * -3, time, ease) 
    elseif curStep == 864+64 then 
        doTweenX('speen', 'noteRot', 0, (stepCrochet/1000)*6, 'cubeInOut') 
        triggerEvent('resetX',''..(stepCrochet/1000)*6, 'cubeInOut')
    elseif curStep == 1840 then 
        if not middlescroll then 
            doTweenX('playerStrumOffset', 'playerStrumOffset', -160, (stepCrochet/1000)*3, 'cubeInOut') 
            doTweenX('opponentStrumOffset', 'opponentStrumOffset', 160, (stepCrochet/1000)*3, 'cubeInOut') 
        else
           
        end

    elseif curStep == 1848 then 
        if not middlescroll then 
            doTweenX('playerStrumOffset', 'playerStrumOffset', -320, (stepCrochet/1000)*3, 'cubeInOut') 
            doTweenX('opponentStrumOffset', 'opponentStrumOffset', 320, (stepCrochet/1000)*3, 'cubeInOut') 
        else 
           
        end

    elseif curStep == 784 then 
        --triggerEvent('setTimeStop', '64050', '68640')
    elseif curStep == 944 then 
        --triggerEvent('setTimeStop', '92370', '92890')
    end

    for i = 0,#woap-1 do --reads a list of steps from the array
        if curStep == woap[i+1] then 

            for j = 0,7 do 
                local ang = 360 
                if j % 4 >= 2 then 
                    ang = -360
                end
                setProperty('noteRot'..j..'.x', ang)
                doTweenX('speen'..j, 'noteRot'..j, 0, stepCrochet/150, 'cubeOut')
            end
            setProperty('scale.x', 1)
            doTweenX('scale', 'scale', 0.7, stepCrochet/150, 'cubeOut')
        end
    end
    --]]
    
    --add events and stuff here



end

function introBuzz()
    if curStep < 192 then 
        if curStep % 16 == 0 or curStep % 16 == 6 then 
            if getProperty('confusion'..startMoveThing..'.angle') == 180 then 
                setProperty('confusion'..startMoveThing..'.angle', 0)
                --setProperty('strumOffset'..startMoveThing..'.angle', 0)
            else 
                setProperty('confusion'..startMoveThing..'.angle', 180)
                --setProperty('strumOffset'..startMoveThing..'.angle', -100)
            end
            
            --doTweenAngle('confusion'..startMoveThing, 'confusion'..startMoveThing, 0, (stepCrochet/1000)*3, 'cubeInOut')
            startMoveThing = (startMoveThing + 1) % 8
        end
    end
end
local frozen = false
function onEvent(tag, val1, val2)
	if tag == 'Toggle BG Freeze' then 
		frozen = not frozen
	end
end
local xOffsetShit = {-100.0, 0.0, 0.0, 100.0}
local yOffsetShit = {0.0, 100.0, -100.0, 0.0}
local trailCount = 0
function goodNoteHit(id, ndata, ntype, isSus)
    --luaDebugMode = true
    if frozen and not isSus then --trails lol
        makeLuaSpriteCopy('trail'..trailCount, 'boyfriend', getProperty('boyfriend.x'), getProperty('boyfriend.y'))
        --setProperty('trail'..trailCount..'.alpha', 0.6)
        addLuaSprite('trail'..trailCount, true)
        setObjectOrder('trail'..trailCount, getObjectOrder('boyfriendGroup'))
        doTweenAlpha('trail'..trailCount, 'trail'..trailCount, 0, crochet*0.001*4, 'cubeOut')
        doTweenX('trailscalex'..trailCount, 'trail'..trailCount..'.scale', 1.2, crochet*0.001*4, 'cubeIn')
        doTweenY('trailscaley'..trailCount, 'trail'..trailCount..'.scale', 1.2, crochet*0.001*4, 'cubeIn')
        doTweenX('trailx'..trailCount, 'trail'..trailCount, getProperty('boyfriend.x')+xOffsetShit[ndata+1], crochet*0.001*4, 'cubeOut')
        doTweenY('traily'..trailCount, 'trail'..trailCount, getProperty('boyfriend.y')+yOffsetShit[ndata+1], crochet*0.001*4, 'cubeOut')

        doTweenColor('trailcol'..trailCount, 'trail'..trailCount, '0xFF2A93E2', 0.001, 'linear')
        trailCount = trailCount + 1 
        if trailCount > 25 then 
            trailCount = 0
        end
    end
end
function opponentNoteHit(id, ndata, ntype, isSus)
    if frozen and not isSus then 
        makeLuaSpriteCopy('trail'..trailCount, 'dad', getProperty('dad.x'), getProperty('dad.y'))
        --setProperty('trail'..trailCount..'.alpha', 0.8)
        addLuaSprite('trail'..trailCount, true)
        setObjectOrder('trail'..trailCount, getObjectOrder('dadGroup'))
        doTweenAlpha('trail'..trailCount, 'trail'..trailCount, 0, crochet*0.001*4, 'cubeOut')
        doTweenX('trailscalex'..trailCount, 'trail'..trailCount..'.scale', 1.2, crochet*0.001*4, 'cubeIn')
        doTweenY('trailscaley'..trailCount, 'trail'..trailCount..'.scale', 1.2, crochet*0.001*4, 'cubeIn')

        doTweenX('trailx'..trailCount, 'trail'..trailCount, getProperty('dad.x')+xOffsetShit[ndata+1], crochet*0.001*4, 'cubeOut')
        doTweenY('traily'..trailCount, 'trail'..trailCount, getProperty('dad.y')+yOffsetShit[ndata+1], crochet*0.001*4, 'cubeOut')

        doTweenColor('trailcol'..trailCount, 'trail'..trailCount, '0xFFFF0000', 0.001, 'linear')
        trailCount = trailCount + 1 
        if trailCount > 25 then 
            trailCount = 0
        end
    end
end