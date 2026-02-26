'use client'

import { Suspense, useState, useRef, useCallback, useEffect, useMemo } from 'react'
import { Canvas, useFrame } from '@react-three/fiber'
import {
  OrbitControls,
  Environment,
  ContactShadows,
  Html,
  useGLTF,
} from '@react-three/drei'
import * as THREE from 'three'

/* ─── Types ─────────────────────────────────────── */

interface Annotation {
  position: [number, number, number]
  label: string
  detail: string
}

interface ModelOption {
  path: string
  label: string
}

interface ProstheticViewerProps {
  /** Array of .glb model paths in /public. */
  models?: ModelOption[]
  annotations?: Annotation[]
  className?: string
}

/* ─── Default models ─────────────────────────────── */

const DEFAULT_MODELS: ModelOption[] = [
  { path: '/models/va-dent-1.glb', label: 'Device 1' },
  { path: '/models/va-dent-2.glb', label: 'Device 2' },
]

/* ─── Default annotations ────────────────────────── */

const DEFAULT_ANNOTATIONS: Annotation[] = [
  {
    position: [0.8, 0.6, 0.3],
    label: 'Palatal Framework',
    detail:
      'Custom-contoured framework designed from CT scan data. Provides structural rigidity while minimizing tissue contact area.',
  },
  {
    position: [-0.6, 0.2, 0.7],
    label: 'Retention Clasps',
    detail:
      'Flexible clasp arms engage undercuts on remaining teeth. Designed for passive insertion with active retention.',
  },
  {
    position: [0.0, -0.3, 0.9],
    label: 'Denture Base',
    detail:
      'Biocompatible resin base seats against edentulous ridge. 3D-printed for precise fit to the veteran\'s anatomy.',
  },
  {
    position: [-0.2, 0.8, -0.3],
    label: 'Occlusal Surface',
    detail:
      'Prosthetic teeth positioned to restore functional occlusion. Material selected for wear resistance and natural appearance.',
  },
]

/* ─── GLB Model with auto-centering & scaling ────── */

function GLBModel({
  path,
  wireframe,
}: {
  path: string
  wireframe: boolean
}) {
  const { scene } = useGLTF(path)
  const groupRef = useRef<THREE.Group>(null)

  // Clone and normalize to a consistent size
  const normalizedScene = useMemo(() => {
    const clone = scene.clone(true)

    // Compute bounding box to normalize size
    const box = new THREE.Box3().setFromObject(clone)
    const size = new THREE.Vector3()
    box.getSize(size)
    const maxDim = Math.max(size.x, size.y, size.z)

    // Scale so the largest dimension is ~2 units
    const targetSize = 2
    if (maxDim > 0) {
      const scale = targetSize / maxDim
      clone.scale.multiplyScalar(scale)
    }

    // Re-center after scaling
    const newBox = new THREE.Box3().setFromObject(clone)
    const center = new THREE.Vector3()
    newBox.getCenter(center)
    clone.position.sub(center)

    return clone
  }, [scene])

  // Apply wireframe to all meshes
  useEffect(() => {
    normalizedScene.traverse((child) => {
      if (child instanceof THREE.Mesh && child.material) {
        if (Array.isArray(child.material)) {
          child.material.forEach((m) => {
            if (m instanceof THREE.MeshStandardMaterial) m.wireframe = wireframe
          })
        } else if (child.material instanceof THREE.MeshStandardMaterial) {
          child.material.wireframe = wireframe
        }
      }
    })
  }, [wireframe, normalizedScene])

  useFrame((_, delta) => {
    if (groupRef.current) {
      groupRef.current.rotation.y += delta * 0.15
    }
  })

  return (
    <group ref={groupRef}>
      <primitive object={normalizedScene} />
    </group>
  )
}

/* ─── Annotation Hotspots ────────────────────────── */

function AnnotationHotspot({
  annotation,
  index,
  active,
  onSelect,
}: {
  annotation: Annotation
  index: number
  active: boolean
  onSelect: (i: number | null) => void
}) {
  return (
    <Html
      position={annotation.position}
      center
      distanceFactor={4}
      zIndexRange={[10, 0]}
    >
      <button
        onClick={(e) => {
          e.stopPropagation()
          onSelect(active ? null : index)
        }}
        className={`
          w-6 h-6 rounded-full border-2 flex items-center justify-center
          text-[10px] font-mono font-bold cursor-pointer
          transition-all duration-200 select-none
          ${
            active
              ? 'bg-copper border-copper text-white scale-125'
              : 'bg-white/10 border-copper/60 text-copper hover:bg-copper/20 hover:scale-110'
          }
        `}
        style={{ backdropFilter: 'blur(8px)' }}
      >
        {index + 1}
      </button>
    </Html>
  )
}

/* ─── Loading Indicator ──────────────────────────── */

function LoadingFallback() {
  return (
    <Html center>
      <div className="flex flex-col items-center gap-3">
        <div className="w-8 h-8 border-2 border-copper/30 border-t-copper rounded-full animate-spin" />
        <span className="font-mono text-xs text-titanium">
          Loading model...
        </span>
      </div>
    </Html>
  )
}

/* ─── Main Viewer Component ──────────────────────── */

export default function ProstheticViewer({
  models = DEFAULT_MODELS,
  annotations = DEFAULT_ANNOTATIONS,
  className = 'aspect-square',
}: ProstheticViewerProps) {
  const [activeModel, setActiveModel] = useState(0)
  const [wireframe, setWireframe] = useState(false)
  const [activeAnnotation, setActiveAnnotation] = useState<number | null>(null)
  const [autoRotate, setAutoRotate] = useState(true)

  const handleAnnotationSelect = useCallback((i: number | null) => {
    setActiveAnnotation(i)
    if (i !== null) setAutoRotate(false)
  }, [])

  return (
    <div className={`relative rounded-xl overflow-hidden ${className}`}>
      {/* Three.js Canvas */}
      <Canvas
        camera={{ position: [0, 0.5, 4], fov: 40 }}
        dpr={[1, 2]}
        gl={{ antialias: true, alpha: true }}
        style={{ background: 'transparent' }}
      >
        <Suspense fallback={<LoadingFallback />}>
          {/* Lighting */}
          <ambientLight intensity={0.5} />
          <directionalLight position={[5, 5, 5]} intensity={0.8} />
          <directionalLight position={[-3, 3, -3]} intensity={0.3} />
          <pointLight position={[0, 2, 0]} intensity={0.2} color="#B87333" />

          {/* Model */}
          <GLBModel
            key={models[activeModel].path}
            path={models[activeModel].path}
            wireframe={wireframe}
          />

          {/* Annotations */}
          {annotations.map((ann, i) => (
            <AnnotationHotspot
              key={i}
              annotation={ann}
              index={i}
              active={activeAnnotation === i}
              onSelect={handleAnnotationSelect}
            />
          ))}

          {/* Environment & Shadows */}
          <ContactShadows
            position={[0, -1.5, 0]}
            opacity={0.3}
            scale={6}
            blur={2}
          />
          <Environment preset="studio" />

          {/* Controls */}
          <OrbitControls
            autoRotate={autoRotate && activeAnnotation === null}
            autoRotateSpeed={1}
            enablePan={false}
            minDistance={1.5}
            maxDistance={8}
            maxPolarAngle={Math.PI * 0.8}
          />
        </Suspense>
      </Canvas>

      {/* Control Bar */}
      <div className="absolute bottom-0 left-0 right-0 p-3 flex items-center justify-between bg-gradient-to-t from-slate-950/90 via-slate-950/50 to-transparent">
        <div className="flex items-center gap-2">
          {/* Model switcher */}
          {models.length > 1 &&
            models.map((model, i) => (
              <ControlButton
                key={model.path}
                active={activeModel === i}
                onClick={() => {
                  setActiveModel(i)
                  setActiveAnnotation(null)
                }}
                label={model.label}
              />
            ))}

          <div className="w-px h-4 bg-white/10 mx-1" />

          <ControlButton
            active={wireframe}
            onClick={() => setWireframe(!wireframe)}
            label="Wireframe"
          />
          <ControlButton
            active={autoRotate}
            onClick={() => setAutoRotate(!autoRotate)}
            label="Rotate"
          />
        </div>

        <span className="font-mono text-[10px] text-titanium/30 hidden sm:block">
          Fusion 360 &rarr; GLB
        </span>
      </div>

      {/* Annotation Detail Panel */}
      {activeAnnotation !== null && annotations[activeAnnotation] && (
        <div className="absolute top-3 right-3 w-64 glass rounded-xl p-4">
          <div className="flex items-start justify-between gap-2 mb-2">
            <div className="flex items-center gap-2">
              <span className="w-5 h-5 rounded-full bg-copper text-white text-[10px] font-mono font-bold flex items-center justify-center flex-shrink-0">
                {activeAnnotation + 1}
              </span>
              <h4 className="font-serif text-sm text-white leading-tight">
                {annotations[activeAnnotation].label}
              </h4>
            </div>
            <button
              onClick={() => setActiveAnnotation(null)}
              className="text-titanium/40 hover:text-white text-lg leading-none flex-shrink-0"
            >
              &times;
            </button>
          </div>
          <p className="text-titanium text-xs leading-relaxed">
            {annotations[activeAnnotation].detail}
          </p>
        </div>
      )}
    </div>
  )
}

/* ─── Control Button ─────────────────────────────── */

function ControlButton({
  active,
  onClick,
  label,
}: {
  active: boolean
  onClick: () => void
  label: string
}) {
  return (
    <button
      onClick={onClick}
      className={`
        px-3 py-1.5 rounded-lg font-mono text-[11px] transition-all duration-200
        ${
          active
            ? 'bg-copper/20 text-copper border border-copper/30'
            : 'bg-white/5 text-titanium/60 border border-white/10 hover:bg-white/10 hover:text-titanium'
        }
      `}
    >
      {label}
    </button>
  )
}
