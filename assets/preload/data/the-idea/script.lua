







local limitx,limity,counterx,countery = 0
function onCreatePost()
    if downscroll then 
		scrollSwitch = -520
	end
    setProperty('incomingAngle.x', 90)

    setProperty('theLimit.alpha', 0)
    setProperty('limitCounter.alpha', 0)

    limitx = getProperty('theLimit.x')
    limity = getProperty('theLimit.y')
    counterx = getProperty('limitCounter.x')
    countery = getProperty('limitCounter.y')

    setObjectCamera('theLimit', 'game')
    setObjectCamera('limitCounter', 'game')

    setProperty('theLimit.x', limitx+830)
    setProperty('theLimit.y', 1035)
    
    setProperty('theLimit.scrollFactor.x', 1)
    setProperty('theLimit.scrollFactor.y', 1)
    setProperty('limitCounter.scrollFactor.x', 1)
    setProperty('limitCounter.scrollFactor.y', 1)
end
function onStepHit()

    if getProperty('endingSong') then 
        return
    end

    if curStep == 40*16 then 
        setProperty('theLimit.alpha', 1)
        setProperty('limitCounter.alpha', 1)
        setObjectCamera('theLimit', 'hud')
        setObjectCamera('limitCounter', 'hud')
        setProperty('theLimit.x', limitx)
        setProperty('limitCounter.x', counterx)
        setProperty('theLimit.y', -300)
        setProperty('limitCounter.y', -300)
        setProperty('theLimit.angle', 1000)
        setProperty('limitCounter.angle', 1000)

        doTweenAngle('theLimit', 'theLimit', 0, crochet/250, 'expoInOut')
        doTweenAngle('limitCounter', 'limitCounter', 0, crochet/250, 'expoInOut')

        doTweenY('theLimitY', 'theLimit', limity, crochet/250, 'expoInOut')
        doTweenY('limitCounterY', 'limitCounter', countery, crochet/250, 'expoInOut')
    elseif curStep == (39*16)+5 then 
        doTweenAlpha('theLimitAlpha', 'theLimit', 1, crochet/1000, 'expoInOut')
    elseif curStep == 634 then
        doTweenAngle('theLimit', 'theLimit', 1000, crochet/1000, 'expoInOut')
        doTweenY('theLimitY', 'theLimit', 0, crochet/1000, 'expoInOut')
    end

    if (curStep >= 40*16 and curStep < 56*16) or (curStep >= 8*16 and curStep < 24*16) then 
        if curStep % 16 == 0 then 
            --[[if (curStep >= 40*16) then 
                for i = 0,3 do 
                    local note = scale(i, 0,3,-2,2)
                    doTweenY('strumOffsety'..i, 'strumOffset'..i, 0, crochet/1000, 'circIn')
                    doTweenAngle('strumOffsetz'..i, 'strumOffset'..i, -(30*note), stepCrochet/250, 'circOut')
                end
    
                for i = 4,7 do 
                    local note = scale(i, 4,7,2,-2)
                    doTweenY('strumOffsety'..i, 'strumOffset'..i, 0, crochet/1000, 'circIn')
                    doTweenAngle('strumOffsetz'..i, 'strumOffset'..i, -(30*note), stepCrochet/250, 'circOut')
                end
            end]]--

            triggerEvent("Add Camera Zoom", 0.08, 0.08)

        elseif curStep % 16 == 4 then 
            --[[if (curStep >= 40*16) then 
                for i = 0,7 do 
                    doTweenY('strumOffsetx'..i, 'strumOffset'..i, -30, crochet/1000, 'circOut')
                    doTweenAngle('strumOffsetz'..i, 'strumOffset'..i, 0, stepCrochet/250, 'circIn')
                end
            end]]--
            triggerEvent("Add Camera Zoom", -0.03, 0.03)
        elseif curStep % 16 == 8 then 
            --[[if (curStep >= 40*16) then 
                for i = 0,3 do 
                    local note = scale(i, 0,3,2,-2)
                    doTweenY('strumOffsety'..i, 'strumOffset'..i, 0, crochet/1000, 'circIn')
                    doTweenAngle('strumOffsetz'..i, 'strumOffset'..i, -(30*note), stepCrochet/250, 'circOut')
                end

                for i = 4,7 do 
                    local note = scale(i, 4,7,-2,2)
                    doTweenY('strumOffsety'..i, 'strumOffset'..i, 0, crochet/1000, 'circIn')
                    doTweenAngle('strumOffsetz'..i, 'strumOffset'..i, -(30*note), stepCrochet/250, 'circOut')
                end
            end]]--
            triggerEvent("Add Camera Zoom", 0.08, 0.08)
        elseif curStep % 16 == 12 then
            --[[if (curStep >= 40*16) then 
                for i = 0,7 do 
                    doTweenY('strumOffsetx'..i, 'strumOffset'..i, -30, crochet/1000, 'circOut')
                    doTweenAngle('strumOffsetz'..i, 'strumOffset'..i, 0, stepCrochet/250, 'circIn')
                end
            end]]--
            triggerEvent("Add Camera Zoom", -0.03, 0.03)
        end
    end
    if curStep == 24*16 or curStep == 56*16 then 
        --[[for i = 0,7 do 
            doTweenY('strumOffsetx'..i, 'strumOffset'..i, 0, crochet/1000, 'circIn')
        end]]--
    end

    



end

--https://stackoverflow.com/questions/5294955/how-to-scale-down-a-range-of-numbers-with-a-known-min-and-max-value
function scale(valueIn, baseMin, baseMax, limitMin, limitMax)
	return ((limitMax - limitMin) * (valueIn - baseMin) / (baseMax - baseMin)) + limitMin
end