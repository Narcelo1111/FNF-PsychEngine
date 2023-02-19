local currentTarget = ''
local followTarget = false
function onEvent(tag, val1, val2)
    if tag == 'setCameraTarget' then
        
        currentTarget = val1
        followTarget = true
    end
end
function onUpdatePost()
    if followTarget then 
        cameraSetTarget(currentTarget)
    end
end
function onBeatHit()
    if curBeat % 4 == 0 then 
        followTarget = false
    end
end

