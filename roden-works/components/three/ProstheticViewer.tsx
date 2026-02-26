'use client'

import { Suspense, useState, useRef, useEffect, useMemo } from 'react'
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
    label: 'Palatal Framework',
    detail:
      'Custom-contoured framework designed from CT scan data. Provides structural rigidity while minimizing tissue contact area.',
  },
  {
    label: 'Retention Clasps',
    detail:
      'Flexible clasp arms engage undercuts on remaining teeth. Designed for passive insertion with active retention.',
  },
  {
    label: 'Denture Base',
    detail:
      'Biocompatible resin base seats against edentulous ridge. 3D-printed for precise fit to the veteran\'s anatomy.',
  },
  {
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

  return (
    <div className="flex flex-col">
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
              autoRotate={autoRotate}
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
      </div>

      {/* Annotation List — below the viewer */}
      {annotations.length > 0 && (
        <div className="grid grid-cols-2 gap-2 mt-3">
          {annotations.map((ann, i) => (
            <button
              key={i}
              onClick={() => setActiveAnnotation(activeAnnotation === i ? null : i)}
              className={`
                text-left rounded-lg p-3 transition-all duration-200 border
                ${
                  activeAnnotation === i
                    ? 'bg-copper/10 border-copper/30'
                    : 'bg-white/[0.02] border-white/5 hover:bg-white/5'
                }
              `}
            >
              <div className="flex items-center gap-2 mb-1">
                <span
                  className={`
                    w-4 h-4 rounded-full text-[9px] font-mono font-bold
                    flex items-center justify-center flex-shrink-0
                    ${
                      activeAnnotation === i
                        ? 'bg-copper text-white'
                        : 'bg-white/10 text-titanium/60'
                    }
                  `}
                >
                  {i + 1}
                </span>
                <span
                  className={`text-xs font-medium ${
                    activeAnnotation === i ? 'text-copper' : 'text-titanium/80'
                  }`}
                >
                  {ann.label}
                </span>
              </div>
              {activeAnnotation === i && (
                <p className="text-titanium/70 text-[11px] leading-relaxed mt-1 pl-6">
                  {ann.detail}
                </p>
              )}
            </button>
          ))}
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
