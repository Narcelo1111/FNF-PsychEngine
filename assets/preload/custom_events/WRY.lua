function onCreatePost()

    local wryname = 'wry'
    local animName = 'RonLv9 WRY'
    local facename = 'face'
    local faceAnim = 'RonFace'
    if getPropertyFromClass('PlayState', 'SONG.player2') == 'woh' then
        wryname = 'woh_wry'
        animName = 'woh WRY'
    end

    --precacheImage('ronald/2phase/frozen/'..wryname)

    makeAnimatedLuaSprite('wryFace','ronald/2phase/'..facename, 0, 340);
    scaleObject('wryFace', 2, 2);
    addAnimationByPrefix('wryFace','wry',faceAnim,24,false)
    addLuaSprite('wryFace',false);
    objectPlayAnimation('wryFace', 'wry', true)
    setProperty('wryFace.alpha', 0)

    makeAnimatedLuaSprite('WRYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY','ronald/2phase/'..wryname, 480, 420);
    addAnimationByPrefix('WRYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY','wry',animName,24,false)
    scaleObject('WRYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY', 2, 2);
    addLuaSprite('WRYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY',false);
    objectPlayAnimation('WRYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY', 'wry', true)
    setProperty('WRYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY.alpha', 0)

end
function onEvent(tag, val1, val2)
	if tag == 'WRY' then 

        setProperty('WRYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY.alpha', 1)
        objectPlayAnimation('WRYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY', 'wry', true)
    
        setProperty('dad.alpha', 0)

        runTimer('wryFaceShow', 24/24);
    
        runTimer('wryThing', 28/24);
    elseif tag == 'WRYEnd' then 
        setProperty('defaultCamZoom', 0.7)
        setProperty('WRYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY.alpha', 0)
        setProperty('wryFace.alpha', 0)
        setProperty('wryFace.x', 0)
        setProperty('dad.alpha', 1)
        cameraFlash('hud', 'FFFFFF', '1',true)
        triggerEvent('Toggle BG Freeze')
    end
end
function onTimerCompleted(tag)
	--Nota stun
    if tag == 'wryThing' then
        cameraFlash('hud', 'FFFFFF', '1',true)
    elseif tag == 'wryFaceShow' then 
        setProperty('fadeFaceIn.alpha', 0.2)
        doTweenAlpha('fadeFaceIn', 'wryFace', 1, 8/24, 'cubeOut')
        doTweenX('faceIn', 'wryFace', 900, 18/24, 'expoOut')
    end
end